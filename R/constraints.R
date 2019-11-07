#' @include util.R
#' @include selections.R
#' @include generics.R

blueprint <- function(selections, mono) {

  ## sort the selections here
  sels <- sorted(sapply(selections, sort_value), TRUE)

  structure(
    list(selections=sels, mono=mono),
    class="blueprint")
}

constraint <- function(..., name) {
  out <- structure(
    list(
      name = name,
      selections = sorted(sapply(selections, sort_value), TRUE)
  ), class = "constraint")
  fit_constraint(out)
}

constraint_order <- function(x, desc=FALSE) {
  mul <- if (desc) -1 else 1
  lapply(x$selections, function(s) if (is.null(s$order)) NULL else s$order * mul)
}

fit_constraint <- function(x) {
  x$blueprints <- list()

  intervals <- Filter(function(x) inherits(x, "interval"), x$selections)

  # check if any interval constraints
  if (length(intervals) > 0) {
    for (i in seq_along(intervals))
      x$blueprints[[i]] <- fit_interval(intervals[[i]], x)
  } else {

    tmp <- list()
    orders <- constraint_order(x)

    for (i in seq_along(x$selections)) {
      sel <- x$selections
      val <- if (inherits(sel, c("identity", "clamp"))) NULL else orders[[i]]
      tmp[[i]] <- fitted_selection(sel, val)
    }

    x$blueprints[[length(x$blueprints) + 1]] <- blueprint(tmp, NULL)
  }

  return(x)
}

check_clamp <- function(x, default=NULL) {
  clamp <- Filter(function(x) inherits(x, "clamp"), x$selections)
  if (len(clamp) > 0) clamp[[1]] else default
}


fit_interval <- function(interval, constraint) {

  clamp <- check_clamp(constraint)

  monos <- switch(
    as.character(
      interval$mono),
    "0"  = c(1, 1, -1, -1),
    "1"  = c(1, 1),
    "-1" = c(-1, -1),
    stop("Invalid montonicity"))

  out <- list()

  for (mi in seq_along(monos)) {
    mono <- monos[[mi]]

    ord <- constraint_order(constraint, desc = (mono != 1))

    if (!is.null(clamp)) {
      interval$ll <- max(ll, clamp$ll)
      interval$ul <- min(ll, clamp$ul)
    }

    pos <- which(sapply(constraint$selections, `==`, interval))
    i <- ord[[pos]]

    vals <- list()
    for (j in ord) {
      if (is.null(j)) {
        vals <- .append(vals, NULL)
      } else if (j < i) {
        vals <- .append(vals, ll - 1 - (i - j))
      } else if (j == i) {
        if (mi %% 2 == 0) {
          vals <- .append(vals, ll - 1)
        } else {
          vals <- .append(vals, ul + 1)
        }
      }
      else {
        val <- .append(ul + 1 - (i - j))
      }
    }

    vals[[pos]] <- list(NULL)

    out[[mi]] <- lapply(seq_along(constraint$selections), function(i) {
      fitted_selection(constraint$selections[[i]], vals[[i]])
    })
  }

  out

}

len.constraint <- function(x) length(x$blueprints)

.transform.constraint <- function(constraint, x) {
  clamp <- check_clamp(constraint, default=clamp(-Inf, Inf))

  out <- lapply(constraint$blueprints, function(bp) {
    res <- rep(NA_real_, length(x))

    for (sel in bp$selections) {

      if (inherits(sel, "clamp")) next
      else res <- .transform(sel, res, clamp)

    }
  })

  monos <- lapply(constraint$blueprints, function(x) if (is.null(x$mono)) 0 else x$mono)

  list(do.call(rbind, out), monos)
}
