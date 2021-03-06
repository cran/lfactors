% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.integer.lfactor.R
\name{as.integer.lfactor}
\alias{as.integer.lfactor}
\title{Integer Vectors from lfactors}
\usage{
\method{as.integer}{lfactor}(x, ...)
}
\arguments{
\item{x}{same as \ifelse{latex}{\code{as.integer}}{\code{\link[base:integer]{as.integer}}}}

\item{\dots}{not used}
}
\description{
Returns integer representation of an lfactor that ignores the values used in
the \code{levels} argument when the lfactor was created and instead returns
an integer representation starting with 1.
}
\details{
This method does not return integer results that are otherwise equal to the
results from as.numeric for compatibility with
\ifelse{latex}{\code{sparse.model.matrix}}{\code{\link[Matrix]{sparse.model.matrix}}}.
}
\examples{
require(lfactors)
# create an example
let <- lfactor(4:12,
               levels=4:12,
               labels=letters[4:12])

as.numeric(let)
#same as as.numeric(4:12)
as.integer(let)
#same as 1:9

}
\seealso{
\ifelse{latex}{\code{as.integer}}{\code{\link[base:integer]{as.integer}}}, \code{\link{as.numeric.lfactor}}
}
