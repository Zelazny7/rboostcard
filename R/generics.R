len <- function(x) UseMethod("len")

in_selection <- function(s, x) UseMethod("in_selection")

sort_value <- function(x) UseMethod("sort_value")

.transform <- function(s, x, ...) UseMethod(".transform")

from_json <- function(x) UseMethod("from_json")

fit_tree <- function(data, w, cons, tf, stump, sel, ...) UseMethod("fit_tree")

update_bin_weights <- function(obj, coef) UseMethod("update_bin_weights")
