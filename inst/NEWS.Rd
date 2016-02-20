\name{NEWS}
\title{NEWS for lfactors}

\section{Changes in version 0.6.0}{
  \itemize{
    \item Fixed a bug in 'droplevels' that would set some levels to NA. This was making lfactors not work well with lm when some levels were unoccupied.
    \item GTE operator works with numeric comparison but still does not work with text comparison.
  }
}

\section{Changes in version 0.5.3}{
  \itemize{
    \item Added 'droplevels' function that allows 'droplevels' to work on an lfactor.
    \item Added 'relevel' function that returns an lfactor.
    \item lfactors can be updated to either a level or an llevel. e.g. month[4] <- 6 has the same effect as month[4] <- "June".
    \item in the [ function the drop argument now works as it does in factor.
    \item in the [ function, not using an 'i' argument now works correctly.
  }
}
