
fit_tree.interval <- function(sel, cons, data, w, tf, stump, min_child_weight, maxdepth) {
  # browser()
  x <- as.numeric(data[,cons$name])
  y <- .transform(stump, tf[[cons$name]])
  f <- in_selection(sel, x)
  d <- data.frame(y=y[f], x=x[f])

  tree <- party::ctree(
    y~x, weights = w[f], data=d, controls = party::ctree_control(
      minbucket = min_child_weight,
      maxdepth = maxdepth))
  add_class(tree_to_bins(tree@tree, sel), "interval_level")
}

`+.interval_level` <- function(e1, e2) {
  out <- rbind(e1, e2)
  i <- sorted(out)
  add_class(out[i,,drop=F], "interval_level")
}

fit_tree.missing_value <- function(sel, cons, data, w, tf, stump, min_child_weight, maxdepth) {
  # browser()
  # if (identical(NROW(stump), 0L)) return(NULL)
  res <- .transform(cons, NA_real_)
  # tf[,stump[[1,"feature"]]] <- res[[stump[[1,"feature"]]]]
  add_class(c(NA_real_, NA_real_, .transform(stump, res[[1]])), "missing_value_level")
}

fit_tree.override <- function(sel, cons, data, w, tf, stump, min_child_weight, maxdepth) {
  res <- .transform(cons, sel$override)
  add_class(c(NA_real_, sel$override, .transform(stump, res[[1]])), "override_level")
}

fit_tree.identity <- fit_tree.interval

fit_tree.clamp <- function(...) add_class(rep(NA_real_, 3), "clamp_level")

binned_variable <- function(l) {
  # l - tree data from fit_tree on selections
  f <- vapply(l, inherits, logical(1), "interval_level")
  structure(class="binned_variable", c(l[!f], list(Reduce(`+`, l[f]))))
}

.transform.binned_variable <- function(obj, x, ...) {
  res <- rep(NA_real_, length(x))
  for (i in seq_along(obj)) res <- .transform(obj[[i]], x, res)
  res
}

.transform.clamp_level <- function(obj, x, res, ...) {
  print("clmapy")
  res
}

.transform.interval_level <- function(obj, x, res, ...) {
  # take level, vector, and result
  # return updated result
  f <- is.na(res)
  vec <- sort(unique(as.numeric(obj[,1:2])))
  res[f] <- obj[,3][findInterval(x[f], vec, left.open = TRUE, all.inside = TRUE)]
  res
}

.transform.missing_value_level <- function(obj, x, res, ...) {
  f <- is.na(res) & is.na(x)
  res[f] <- obj[[3]]
  res
}

.transform.override_level <- function(obj, x, res, ...) {
  f <- is.na(res) & x == obj[[2]]
  res[f] <- obj[[3]]
  res
}

update_bin_weights.binned_variable <- function(obj, coef) {
  for (i in seq_along(obj)) obj[[i]] <- update_bin_weights(obj[[i]], coef)
  obj
}

update_bin_weights.default <- function(obj, coef) {
  obj[[3]] <- obj[[3]]*coef
  obj
}


update_bin_weights.interval_level <- function(obj, coef) {
  obj[,3] <- obj[,3]*coef
  obj
}
