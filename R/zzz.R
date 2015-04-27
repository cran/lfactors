.onLoad <- function(libname, pkgname) {
	setOldClass("lfactor")
  setGeneric("match", where=.GlobalEnv)
	setMethod("match", signature(x="lfactor"), mlfactor, where=.GlobalEnv)
	setMethod("match", signature(table="lfactor"), mlfactor, where=.GlobalEnv)
	setMethod("match", signature(x="lfactor", table="lfactor"), mlfactor, where=.GlobalEnv) 
  setGeneric("%in%", where=.GlobalEnv)
	setMethod("%in%", signature(x="lfactor"), inlf, where=.GlobalEnv)
	setMethod("%in%", signature(table="lfactor"), inlf, where=.GlobalEnv)
	setMethod("%in%", signature(x="lfactor", table="lfactor"), inlf, where=.GlobalEnv) 
}