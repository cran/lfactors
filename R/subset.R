#' @method [ lfactor
#' @export
"[.lfactor" <- function(x,i) {
  xo <- x
  class(xo) <- "factor"
  res <- xo[i]
  attr(res,"llevels") <- llevels(x)
  class(res) <- c("lfactor", "factor")
  res
}
