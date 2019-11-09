#' @include constraints.R

setClassUnion("ListOrCharacter", members = c("list", "character"))

Boostcard <- setRefClass(
  "Boostcard",
  fields = list(
    constraints = "ListOrCharacter", # Union[str, List[constraint]]
    bins = "list",
    objective = "character",
    n_estimators = "integer",
    eta = "numeric",
    subsample = "numeric",
    gamma = "numeric",
    min_child_weight = "numeric",
    max_leaf_nodes = "integer"),

  methods = list(
    initialize = function(constraints, objective="binary:logitraw",
                          n_estimators=100L, eta=0.3, subsample=0.80, gamma=0.0,
                          min_child_weight = 1L, max_leaf_nodes = 8L) {

      constraints <<-
        if (is.list(constraints)) constraints
        else Constraints.from_json(constraints)

      .self$objective <<- objective
      .self$n_estimators <<- n_estimators
      .self$eta <<- eta
      .self$subsample <<- subsample
      .self$gamma <<- gamma
      .self$min_child_weight <<- min_child_weight
      .self$max_leaf_nodes <<- max_leaf_nodes
      .self$bins <<- list()

      .self

    },
    fit = function(X, y, w=rep(1, length(y)), ...) {

      ## fit the xgb model
      # transform input data through constraints
      res <- .self$transform(X)

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

      ## dump and stump
      stumps <- lapply(split_xgb_outputs(fit, res$ncols), stump)

      ## generate predictions and trees for selections
      preds <- lapply(stumps, .transform, tf)

      ## create final bins using rpart (or faster) -- preserving selection info
      ## TODO: make this a function and optionally call
      .self$fit_trees(stumps, X, tf, w, ...)

      ## post-process using glmnet to calibrate
      ## TODO: make this a function and optionally call
      preds

    },
    fit_trees = function(stumps, X, tf, w, method="class") {
      # stumps - list of stumps
      # X - original data
      # tf - transformed data
      
      for (i in seq_along(.self$constraints)) {
        cons <- .self$constraints[[i]]
        res <- lapply(cons$selections, fit_tree, cons, X, w, tf, stumps[[i]], .self$min_child_weight)
        
        ## TODO: create a bin class and dump these there
        f <- sapply(res, inherits, "interval_level")
        intervals <- do.call(rbind, res[f])
        i <- sorted(intervals)
        .self$bins[[cons$name]] <- rbind(do.call(rbind, res[!f]), intervals[i,,drop=F])
      }
    },
    transform = function(X) {

      xs <- list()
      monos <- list()

      for (i in seq_along(.self$constraints)) {
        constraint <- .self$constraints[[i]]
        res <- .transform(constraint, X[,constraint$name])
        xs <- .append(xs, res[[1]])
        monos <- .append(monos, res[[2]])
      }

      list(features=xs, mono=monos, ncols=lapply(xs, NCOL))
    }
  )
)


fit_tree.interval <- function(sel, cons, data, w, tf, stump, min_child_weight) {
  y <- .transform(stump, tf)
  f <- in_selection(sel, data[,cons$name])
  d <- data.frame(y=y[f], x=data[f,cons$name])
  
  tree <- party::ctree(
    y~x, weights = w, data=d, controls = party::ctree_control(minbucket = min_child_weight))
  add_class(tree_to_bins(tree@tree, sel), "interval_level")
}

fit_tree.missing_value <- function(sel, cons, data, w, tf, stump, min_child_weight) {
  res <- .transform(cons, NA_real_)
  add_class(c(NA_real_, NA_real_, .transform(stump, res[[1]])), "missing_value_level")
}

fit_tree.override <- function(sel, cons, data, w, tf, stump, min_child_weight) {
  res <- .transform(cons, sel$override)
  add_class(c(NA_real_, sel$override, .transform(stump, res[[1]])), "override_level")
}

fit_tree.identity <- fit_tree.interval
