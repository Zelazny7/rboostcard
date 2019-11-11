#' @include generics.R

stump <- function(feature_data) {
  # browser()
  # Feature is a matrix that looks like this
  #
  #      feature threshold       left        right
  # [1,]       7       2.5 0.05022478 -0.021329313
  # [2,]       7       2.5 0.02802211 -0.019611036

  structure(feature_data, class="stump")
}

.transform.stump <- function(stump, x, ...) {

  # X should be the transformed chunk of data just for this column

  res <- numeric(NROW(x))
  if (NROW(stump) == 0) return(res)
  for (i in seq.int(nrow(stump))) {
    vec <- stump[i,]
    idx <- stump[[i,1]]
    res <- res + ifelse(x[,idx] < vec['threshold'], vec['left'], vec['right'])
  }
  res
}
