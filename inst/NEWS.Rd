\name{NEWS}
\title{NEWS for lfactors}

\section{Changes in version 0.5.3}{
  \itemize{
    \item Added 'droplevels' function that allows 'droplevels' to work on an lfactor.
    \item Added 'relevel' function that returns an lfactor.
    \item lfactors can be updated to either a level or an llevel. e.g. month[4] <- 6 has the same effect as month[4] <- "June".
    \item in the [ function the drop argument now works as it does in factor.
    \item in the [ function, not using an 'i' argument now works correctly.
  }
}
