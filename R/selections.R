#' @include util.R
#' @include generics.R

selection <- function(priority, order = 0, value = NA_real_) {
  structure(
    list(priority=as.numeric(priority), order=as.numeric(order), value=value),
    class = "selection")
}

identity <- function(order = 0) {
  add_class(selection(priority=100, order=order, value=NULL), "identity")
}

in_selection.identity <- function(s, x) TRUE

clamp <- function(ll, ul, order = 0) {
  out <- selection(priority=2, order=-Inf, value=NULL)
  out$ll <- ll
  out$ul <- ul
  add_class(out, "clamp")
}

in_selection.clamp <- function(s, x) TRUE

interval <- function(ll, ul, order = 0, mono = 0) {
  out <- selection(priority=0)
  out$ll <- as.numeric(ll)
  out$ul <- as.numeric(ul)
  out$order <- order
  out$mono <- mono
  add_class(out, "interval")
}

in_selection.interval <- function(s, x) !is.na(x) & (x > s$ll) & (x <= s$ul)


override <- function(override, order = 0) {
  out <- selection(priority = 3, order=order)
  out$override <- override
  add_class(out, "override")
}

in_selection.override <- function(s, x) (as.numeric(x) == s$override) & !is.na(x)

missing_value <- function(order = 0) {
  add_class(selection(priority=1, order=order), "missing_value")
}

in_selection.missing_value <- function(s, x) is.na(x)

fitted_selection <- function(selection, value = NULL) {
  selection['value'] <- list(value)
  add_class(selection, "fitted_selection")
}

.transform.fitted_selection <- function(s, x, result, clamp) {

  if (inherits(s, "interval")) {
    clipped <- clip(x, clamp$ll, clamp$ul)
    f <- in_selection(s, clipped) & is.na(result)
    x[f] <- clipped[f]
  }

  val <- if (is.null(s$value)) x else s$value
  f <- in_selection(s, x) & is.na(result)
  ifelse(f, val, result)
}

sort_value.interval <- function(s) c(s$priority, s$order, s$ll)
sort_value.selection <- function(s) c(s$priority, s$order, -Inf)


`==.selection` <- function(e1, e2) {
  if (!identical(length(e1), length(e2))) FALSE
  else all(sapply(seq_along(e1), function(i) identical(e1[[i]], e2[[i]])))
}


Selection.from_list <- function(l) {
  if (is.null(l$order)) l$order <- 0
  with(l, switch(
    type,
    "interval" = interval(ll, ul, order, mono),
    "override" = override(override, order),
    "missing_value" = missing_value(order),
    "identity" = identity(),
    "clamp" = clamp(ll, ul, order),
    stop("Unsupported selection!", l$type))
  )

}

# TODO: not sure this is needde
Selection.from_json <- function(json) {
  decoded <- jsonlite::fromJSON(json)
  Selection.from_list(decoded)
}



json <- '
{
  "type" : "interval",
  "ll": 0,
  "ul": 20,
  "mono": 1
}
'

# Selection.from_list(list(type="interval", ll=0, ul=10))

# Selection.from_json(json)

