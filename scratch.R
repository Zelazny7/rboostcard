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

y <- as.integer(iris$Species == "virginica")
bst <- new("Boostcard", constraints="config.json", min_child_weight=25L, max_depth=4L)
d <- sample(nrow(titanic), nrow(titanic)*0.66)
v <- setdiff(seq(nrow(titanic)), d)

bst$fit(titanic[d,-1], titanic$Survived[d])

p <- bst$predict(titanic)

# library(ks)
# ksd <- data.frame(score=-p, y=titanic$Survived, sample=ifelse(seq(nrow(titanic)) %in% d, "dev", "val"))
# kst <- ks_table(y~score|sample, data = ksd, number_bins = 20)

library(onyx)

mod <- bin(titanic[d,-1], titanic$Survived[d], mono=2)
mod$set_step(lvl=1)
mod$fit()

val <- mod$predict(titanic[-1])

ksd <- data.frame(onyx=val, score=p, y=titanic$Survived, sample=ifelse(seq(nrow(titanic)) %in% d, "dev", "val"))
kst <- ks_table(y~score+onyx|sample, data = ksd, number_bins = 20)

bst$bins$Age


bst <- new("Boostcard")


f <- sapply(titanic, is.factor)
titanic[f] <- lapply(titanic[f], as.numeric)

bst$fit(titanic[-1], titanic$Survived)

p <- bst$predict(iris)

bst$transform(iris[-5])

res <- bst$fit(X=iris[-5], y=titanic$Survived)

do.call(cbind, res$features)


tree <- party::ctree(iris$Petal.Width~iris$Sepal.Length,
                     controls = party::ctree_control(minbucket = 25))

tree_to_bins(tree@tree, identity())

tree@tree[["children"]]

tree


## tests for constraints ....

c1 <- constraint(
  clamp(2, 5),
  interval(1.9, 5),
  missing_value(), name="v1")

.transform(c1, c(NA, 1:10))

## this shit is pissing me off...!
