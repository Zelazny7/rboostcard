#' @include constraints.R
#' @include bin.R

setClassUnion("ListOrCharacterOrNULL", members = c("list", "character", "NULL"))

## map xgboost objectives to glmnet families

xgb2glmnet <- function(x) {
  family <- gsub("(^\\w+):..*$", "\\1", x)
  switch(
    family,
    "binary" = "binomial",
    "reg" = "gaussian",
    stop("Objective not supported: ", x))
}

Boostcard <- setRefClass(
  "Boostcard",
  fields = list(
    constraints = "ListOrCharacterOrNULL", # Union[str, List[constraint]]
    bins = "list",
    objective = "character",
    n_estimators = "integer",
    eta = "numeric",
    subsample = "numeric",
    gamma = "numeric",
    intercept = "numeric",
    min_child_weight = "numeric",
    max_depth = "integer"),

  methods = list(
    initialize = function(constraints=NULL, objective="binary:logitraw",
                          n_estimators=500L, eta=0.1, subsample=0.80, gamma=0.0,
                          min_child_weight = 1L, max_depth = 4L) {

      if (!is.null(constraints)) {
        .self$constraints <<-
          if (is.list(constraints)) constraints
          else Constraints.from_json(constraints)
      }

      .self$objective <<- objective
      .self$n_estimators <<- n_estimators
      .self$eta <<- eta
      .self$subsample <<- subsample
      .self$gamma <<- gamma
      .self$min_child_weight <<- min_child_weight
      .self$max_depth <<- max_depth

      .self$bins <<- list()
      .self$intercept <<- 0

      .self

    },
    fit = function(X, y, w=rep(1, length(y)), ...) {

      ## fit the xgb model
      # transform input data through constraints
      res <- .self$transform(X)

      # browser()
      tf <- do.call(cbind, res$features)
      monos <- sprintf("(%s)", paste0(unlist(res$mono), collapse=","))

      params <- list(
        tree_method = "hist",
        grow_policy = "lossguide",
        max_depth = 1,  # hard-coded
        objective = .self$objective,
        eta = .self$eta,
        subsample = .self$subsample,
        gamma = .self$gamma,
        min_child_weight = .self$min_child_weight,
        monotone_constraints = monos)

      # fit the xgboost model
      x_train <- xgboost::xgb.DMatrix(tf, label=y, weight=w)
      fit <- xgboost::xgb.train(params, x_train, nrounds = .self$n_estimators)

      ## dump and stump -- make these all 1-indexed
      stumps <- lapply(split_xgb_outputs(fit, res$ncols), stump)

      ## create final bins using party -- preserving selection info
      .self$fit_trees(stumps, X, res$features, w, ...)

      ## post-process using glmnet to calibrate
      preds <- mapply(.transform, .self$bins, X[names(.self$bins)], SIMPLIFY = TRUE)

      family <- xgb2glmnet(.self$objective)
      fit <- glmnet::cv.glmnet(preds, y, alpha=0, lower.limits=0, family=family)

      ## update bins with coefficients
      update_bin_weights(fit)

      invisible(NULL)

    },

    update_bin_weights = function(fit, ...) {
      coefs <- as.list(coef(fit, s="lambda.min")[,1])
      v <- names(.self$bins)
      .self$bins <- mapply(rboostcard::update_bin_weights, bst$bins[v], coefs[v], SIMPLIFY = F)
      .self$intercept <- coefs[["(Intercept)"]]
    },

    fit_trees = function(stumps, X, tf, w, method="class") {
      # stumps - list of stumps
      # X - original data
      # tf - transformed data
      for (i in seq_along(.self$constraints)) {
        cons <- .self$constraints[[i]]
        res <- lapply(cons$selections, fit_tree, cons, X, w, tf, stumps[[i]],
                      .self$min_child_weight, .self$max_depth)

        ## TODO: create a bin class and dump these there
        .self$bins[[cons$name]] <- binned_variable(res)
      }
    },
    transform = function(X) {

      xs <- list()
      monos <- list()

      for (v in names(.self$constraints)) {
        constraint <- .self$constraints[[v]]
        res <- .transform(constraint, X[[v]])
        xs[[v]] <- res[[1]]
        monos[[v]] <- res[[2]]
      }

      list(features=xs, mono=monos, ncols=lapply(xs, NCOL))
    },

    predict = function(X, what="value") {
      cols <- mapply(.transform, .self$bins, X[names(.self$bins)], SIMPLIFY = TRUE)
      switch(
        what,
        "value" = rowSums(cols) + .self$intercept,
        "features" = cols,
        stop("requested prediction type not valid: ", what)
      )
    }
  )
)

