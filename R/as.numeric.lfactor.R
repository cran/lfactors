#' @method as.numeric lfactor
#' @export
as.numeric.lfactor <- function(x, ...) {
  xi <- switchllevels(x)
  as.numeric(xi)
}
