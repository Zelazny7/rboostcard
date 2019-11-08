#' @include util.R
#' @include selections.R
#' @include generics.R

blueprint <- function(selections, mono) {

  ## sort the selections here
  i <- sorted(t(sapply(selections, sort_value)), TRUE)

  structure(
    list(selections=selections[i], mono=mono),
    class="blueprint")
}

constraint <- function(..., name) {
  sels <- list(...)
  i <- sorted(t(sapply(sels, sort_value)), TRUE)
  out <- structure(
    list(
      name = name,
      selections = sels[i]
  ), class = "constraint")
  fit_constraint(out)
}

constraint_order <- function(x, desc=FALSE) {
  mul <- if (desc) -1 else 1
  lapply(x$selections, function(s) if (is.null(s$order)) NULL else s$order * mul)
}

fit_constraint <- function(x) {
  blueprints <- list()

  intervals <- Filter(function(x) inherits(x, "interval"), x$selections)

  # check if any interval constraints
  if (length(intervals) > 0) {
    for (i in seq_along(intervals))
      blueprints <- append(blueprints, fit_interval(intervals[[i]], x))
      # x$blueprints[[i]] <- fit_interval(intervals[[i]], x)
  } else {

    tmp <- list()
    orders <- constraint_order(x)

    for (i in seq_along(x$selections)) {
      sel <- x$selections[[i]]
      val <- if (inherits(sel, c("identity", "clamp"))) NULL else orders[[i]]
      tmp[[i]] <- fitted_selection(sel, val)
    }


    #x$blueprints[[length(x$blueprints) + 1]] <- blueprint(tmp, NULL)

    blueprints <- append(blueprints, list(blueprint(tmp, NULL)))
  }

  #x$blueprints <- unlist(x$blueprints, recursive = F)
  x$blueprints <- blueprints
  return(x)
}

check_clamp <- function(x, default=NULL) {
  clamp <- Filter(function(x) inherits(x, "clamp"), x$selections)
  if (length(clamp) > 0) clamp[[1]] else default
}


fit_interval <- function(interval, constraint) { # -> List[blueprint]

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

    ll <- max(interval$ll, if (is.null(clamp)) clamp$ll else -Inf)
    ul <- min(interval$ul, if (is.null(clamp)) clamp$ul else Inf)

    pos <- which(sapply(constraint$selections, `==`, interval))
    i <- ord[[pos]]

    vals <- list()
    for (j in ord) {
      if (is.null(j)) { # clamp selection
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
        vals <- .append(vals, ul + 1 - (i - j))
      }
    }

    vals[pos] <- list(NULL)

    fs <- lapply(seq_along(constraint$selections), function(i) {
      fitted_selection(constraint$selections[[i]], vals[[i]])
    })

    out[[mi]] <- blueprint(fs, mono)
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
      else res <- .transform(sel, x, res, clamp)
    }
    res
  })

  monos <- lapply(constraint$blueprints, function(x) if (is.null(x$mono)) 0 else x$mono)

  list(do.call(cbind, out), monos)
}



Constraint.from_list <- function(l, name) {
  do.call(constraint, c(lapply(l, Selection.from_list), name = name))
}

Constraints.from_json <- function(json) {
  decoded <- jsonlite::read_json(json, simplifyVector = F)
  mapply(Constraint.from_list, decoded, names(decoded), SIMPLIFY = F)
}
