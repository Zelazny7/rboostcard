library(xgboost)
library(timbr)
data(titanic, package="onyx")

p <- prep(titanic[-1])
x <- predict(p, titanic)

params <- list(
  min_child_weight=1,
  eta=0.1,
  gamma=0.0,
  max_depth=1,
  subsample=0.80,
  objective='binary:logitraw'
)

x_train <- xgb.DMatrix(x, label=titanic$Survived)
xgb <- xgboost::xgb.train(params, x_train, nrounds = 500)


bst <- new("Boostcard", constraints="config.json")


split_data <- split_xgb_outputs(xgb, res$ncols)


res <- bst$fit(X=x, y=titanic$Survived)

do.call(cbind, res$features)
