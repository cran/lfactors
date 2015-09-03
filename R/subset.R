#' @method [ lfactor
#' @export
"[.lfactor" <- function(x,i, ..., drop=FALSE) {
  xo <- x
  class(xo) <- "factor"
  if(!missing(i)) {
    res <- xo[i,...,drop=drop]
  } else {
    res <- xo[...,drop=drop]
  }
  attr(res,"llevels") <- llevels(x)
  class(res) <- c("lfactor", "factor")
  if(drop) {
    res <- droplevels(res)
  }
  res
}
