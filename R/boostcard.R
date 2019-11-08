#' @include constraints.R

setClassUnion("ListOrCharacter", members = c("list", "character"))

Boostcard <- setRefClass(
  "Boostcard",
  fields = list(
    constraints = "ListOrCharacter", # Union[str, List[constraint]]
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

      .self

    },
    fit = function(X, y, w=rep(1, length(y)), ...) {

      ## fit the xgb model
      # transform input data through constraints
      res <- .self$transform(X)

      X <- do.call(cbind, res$features)
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
      x_train <- xgboost::xgb.DMatrix(X, label=y, weight=w)
      fit <- xgboost::xgb.train(params, x_train, nrounds = .self$n_estimators)

      ## dump and stump
      stumps <- lapply(split_xgb_outputs(fit, res$ncols), stump)

      ## generate predictions and trees for selections
      preds <- lapply(stumps, .transform, X)

      ## create final bins using rpart (or faster) -- preserving selection info


      ## post-process using glmnet to calibrate

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



