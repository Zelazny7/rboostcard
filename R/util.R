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
