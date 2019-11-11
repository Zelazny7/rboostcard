sorted <- function(m, reverse=FALSE) {
  cols <- lapply(seq.int(ncol(m)), function(i) m[,i])
  do.call(order, cols)
}

.append <- function(l, val) {
  l[[length(l) + 1]] <- val
  l
}

clip <- function(x, ll, ul) {
  stopifnot(ul >= ll)
  out <- x
  out[x > ul] <- ul
  out[x < ll] <- ll
  out
}

add_class <- function(x, klass) {
  class(x) <- unique(c(class(x), klass))
  x
}


## TODO: implement utils from pyboostcard
lengths_to_indices <- function(lens) { # -> List[int]
  # (lens: List[int])
  split(seq(sum(z <- unlist(lens))), rep(seq(length(lens)), z))
}

split_xgb_outputs <- function(xgb, lens) {
  m <- get_xgb_features_and_values(xgb)
  lapply(lengths_to_indices(lens), function(ids) {
    # browser()
    res <- m[m[,'feature',drop=F] %in% ids,,drop=F]
    ## make the feautres 1-indexed
    res[,'feature'] <- res[,'feature'] - min(ids) + 1
    res
  })
}

get_xgb_features_and_values <- function(xgb) {
  dump <- xgboost::xgb.dump(xgb, with_stats=TRUE)

  '> head(dump)
   [1] "booster[0]"
   [2] "0:[f1<1.5] yes=1,no=2,missing=1,gain=191.553177,cover=176"
   [3] "1:leaf=0.0917647108,cover=62.75"
   [4] "2:leaf=-0.124726474,cover=113.25"
   [5] "booster[1]"
   [6] "0:[f1<1.5] yes=1,no=2,missing=1,gain=171.319641,cover=175.177063"'

  pat <- "\\[f([0-9]+)<(-?[0-9]+.?[0-9-e]*)\\]"
  matches <- regexec(pat, dump, perl=TRUE)

  ## extract features and thresholds - 1-indexed features
  features <- sapply(regmatches(dump, matches), function(x) {
    if (identical(x, character(0))) NULL
    else c(feature=as.integer(x[[2]]) + 1L, threshold=as.numeric(x[[3]]))
  })

  ## extract leaf values
  pat <- "^[12]:leaf=(-?[0-9]+.[0-9-e]+),"
  leaves <- regexec(pat, dump, perl=TRUE)
  leaves <- Filter(function(x) length(x) > 0, regmatches(dump, leaves))
  values <- matrix(as.numeric(sapply(leaves, '[[', 2)), ncol = 2, byrow = T,
                  dimnames = list(NULL, c("left","right")))

  # browser()
  ## zip them up
  cbind(do.call(rbind, features), values)
}

tree_to_bins <- function(tree, sel) {
  ## take ctree tree@tree as input (list)
  ## inveral to bound with ll and ul
  recurse <- function(tree, bounds, res = list()) {
    if (tree$terminal) return(list(c(bounds, tree$prediction)))
    else c(res,
           recurse(tree$left, c(bounds[[1]], tree$psplit$splitpoint)),
           recurse(tree$right, c(tree$psplit$splitpoint, bounds[[2]])))
  }
  bounds <- if (is.null(sel$ll)) c(-Inf, Inf) else c(sel$ll, sel$ul)
  res <- recurse(tree, bounds=bounds)
  do.call(rbind, res)
}


# def lengths_to_indices(lens: List[int]) -> List[List[int]]:
#   """[2, 3, 2] -> [[0,1], [2,3,4], [5,6]]"""
# out = []
# curr = 0
# for l in lens:
#   out.append(list(range(curr, curr + l)))
# curr = curr + l
# return out
