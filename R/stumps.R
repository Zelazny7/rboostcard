#' @include generics.R

stump <- function(feature_data) {
  # Feature is a matrix that looks like this
  #
  #      feature threshold       left        right
  # [1,]       7       2.5 0.05022478 -0.021329313
  # [2,]       7       2.5 0.02802211 -0.019611036

  structure(feature_data, class="stump")
}

.transform.stump <- function(stump, x, ...) {
  # browser()
  res <- numeric(NROW(x))
  for (i in seq.int(nrow(stump))) {
    vec <- stump[i,]
    res <- res + ifelse(x[,vec['feature']] < vec['threshold'], vec['left'], vec['right'])
  }
  res
}
