# Rd in man, not roxygen
#' @export
inlf <- function(x, table) {
  if(inherits(x, "lfactor")) {
  	m1 <- match(x=as.character(x), table=as.character(table), nomatch=0, incomparables=NULL)
  	m2 <- match(x=as.character(switchllevels(x)), table=as.character(table), nomatch=0, incomparables=NULL)
  } else {
    m1 <- match(x=as.character(x), table=as.character(table), nomatch=0, incomparables=NULL)
    m2 <- match(x=as.character(x), table=as.character(switchllevels(table)), nomatch=0, incomparables=NULL)
  }
  ((m1 > 0) | (m2 > 0))
}

methods::setGeneric("%in%")
methods::setMethod("%in%", methods::signature(x="lfactor"), inlf)
methods::setMethod("%in%", methods::signature(table="lfactor"), inlf)
methods::setMethod("%in%", methods::signature(x="lfactor", table="lfactor"), inlf) 


