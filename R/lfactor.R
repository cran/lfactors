#' @title Lfactors
#'
#' @description \code{lfactor} creates a factor that can be compared to its levels or labels.
#'
#' @param x a vector of data, in numeric format.
#' @param levels a vector of levels in x. Unlike factor, these must be numeric. 
#' @param labels a vector of labels for the levels. This vector must be either
#'               characters that cannot be cast as numeric or characters that are
#'               equal to the level when cast as numeric.
#' @param \dots arguments passed to \code{\link[base]{factor}}.
#'
#' @details 
#' An lfactor can be compared to the levels or the labels (see the `Examples'). Because of that,
#' the levels must be numeric and the labels must be either not castable as numeric or equal to
#' the levels when cast as numeric.
#' 
#' An lfactor is, essentialy, a factor that remembers the levels as well as the labels argument.
#' Note that all of the arguments are passed to \code{\link[base]{factor}}. Because lfactor imposes
#' some additional constraints on the types of levels and labels and stores additional information,
#' an lfactor both uses more memory than and is, in some ways, more limited than a factor.
#' 
#' @return
#' An object of class lfactor that also implement \code{\link[base]{factor}}
#'
#' @seealso \code{\link[base]{factor}}
#' 
#' @examples
#' # make an example lfactor object
#' mon <- lfactor(1:12,
#'                levels=1:12,
#'                labels=c("Jan", "Feb", "Mar", "Apr", "May","Jun",
#'                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#' # print out the lfactor
#' mon
#' # compare to label
#' mon == "Feb"
#' # Compare to level
#' mon == 2
#' # Show that the == works correctly
#' all.equal(mon == "Feb", mon == 2)
#' # Show that the != works correctly
#' all.equal(mon != "Feb", mon != 2)
#' # also works when the vector is not the lfactor
#' all.equal(mon[3] == c("Jan", "Feb", "Mar"), mon[3] == 1:3)
#' 
#' # or when both the lfactor and the object being compare to are vectors
#' all.equal(mon[1:2] == c("Feb", "Tuesday"), mon[1:2] == c(2,-4) )
#' 
#' # similar to Ops.factor, this gives a helpful warning and NA results
#' mon >= "Jan" 
#' 
#' # %in% works correctly
#' all.equal(mon %in% c(2, 3), mon %in% c("Feb", "Mar"))
#' # and when the lfactor is on the right
#' all.equal(c(-4, 14,3,10) %in% mon, c("not a month", "Third December","Mar","Oct") %in% mon)
#' # and when both left and right are lfactors
#' all.equal(mon %in% mon, rep(TRUE,12))
#'
#' 
#' @export
lfactor <- function(x, levels, labels=levels, ...) {
  if(! class(levels) %in% c("integer", "numeric") ) {
    stop(paste0("The ",sQuote("levels"), " argument must be of class integer or numeric."))
  }
  goodlabs <- rep(FALSE, length(labels)) 
  suppressWarnings(nlabs <- as.numeric(labels))
  goodlabs[is.na(nlabs)] <- TRUE
  goodlabs[!is.na(nlabs) & nlabs == levels] <- TRUE
  if( sum(goodlabs) < length(labels)) {
    stop(paste0("The ",sQuote("levels"), " and ", sQuote("labels"), " arguments must either be identical or the labels must not be numbers."))
  }
  res <- factor(x, levels=levels, labels=labels, ...)
  attr(res,"llevels") <- levels
  class(res) <- c("lfactor", "factor")
  res
}
