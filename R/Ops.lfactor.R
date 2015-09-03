#' @method Ops lfactor
#' @export
Ops.lfactor <- function(e1,e2) {
  if(.Generic %in% c("<", "<=", ">=", ">")) {
    if(inherits(e1, "lfactor")) {
      e1 <- as.numeric(e1)
    }
    if(inherits(e2, "lfactor")) {
      e2 <- as.numeric(e2)
    }
    if(inherits(e1, "numeric") & inherits(e2, "numeric")) {
      return(eval(call(.Generic,e1,e2)))
    }
  }
  if(! .Generic %in% c("==", "!=")) {
    return(NextMethod(e1,e2))
  }
  e2 <- as.character(e2)
  lvl <- levels(e1)
  llvl <- llevels(e1)
  e1 <- factor(e1)
  for(oli in 1:length(llvl)) {
    e2i <- e2 %in% llvl[oli]
    e2[e2i] <- lvl[oli]
  }
  return(NextMethod(e1,e2))
}
