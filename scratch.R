library(xgboost)
library(timbr)
# data(titanic, package="onyx")

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

json <- '{
"Sepal.Length" : [
{"type": "missing_value"},
{"type": "interval", "ll": 0, "ul":10, "mono": -1}
],
"Sepal.Width"  : [{"type": "identity"}],
"Petal.Length" : [{"type": "identity"}],
"Petal.Width"  : [{"type": "identity"}]
}'


l <- jsonlite::fromJSON(json, simplifyVector = F)
constraints <- mapply(Constraint.from_list, l, names(l), SIMPLIFY = F)

bst <- new("Boostcard", constraints=constraints)
y <- as.integer(iris$Species == "virginica")
p <- bst$fit(iris[-5], y)

bst$transform(iris[-5])

res <- bst$fit(X=iris[-5], y=titanic$Survived)

do.call(cbind, res$features)


tree <- party::ctree(iris$Petal.Width~iris$Sepal.Length,
                     controls = party::ctree_control(minbucket = 25))

tree_to_bins(tree@tree, identity())

tree@tree[["children"]]

tree
