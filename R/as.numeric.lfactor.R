#' @method as.numeric lfactor
#' @export
as.numeric.lfactor <- function(x, ...) {
  as.numeric(as.character(switchllevels(x)))
}

#' @method as.integer lfactor
#' @export
as.integer.lfactor <- function(x, ...) {
  as.integer(as.character(switchllevels(x)))
}

#' @method as.double lfactor
#' @export
as.double.lfactor <- function(x, ...) {
  as.double(as.character(switchllevels(x)))
}

#setMethod("as.numeric", signature(x="lfactor"), as.numeric.lfactor)
#setMethod("as.integer", signature(x="lfactor"), as.numeric.lfactor)
#setMethod("as.double", signature(x="lfactor"), as.numeric.lfactor)

