
binned_variable <- function(l) {
  # l - tree data from fit_tree on selections
  f <- sapply(l, inherits, "interval_level")
  ints <- do.call(rbind, l[f])
  i <- sorted(ints)
  structure(class="binned_variable", rbind(do.call(rbind, l[!f]), ints[i,,drop=F]))
}