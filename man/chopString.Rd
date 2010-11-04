\name{chopString}
\alias{chopString}
\title{
 Truncate a string and add an ellipsis
}
\description{
 Truncates one or more strings to a specified length, adding an ellipsis (...)
 to those strings that are truncated.
}
\usage{
 chopString(x,maxlen=20)
}
\arguments{
 \item{x}{An object.}
 \item{maxlen}{The maximum length of the returned string(s).}
}
\value{The string(s) passed as \samp{x} now with a maximum length of \samp{maxlen}.}
\author{Jim Lemon}
\seealso{\link{substr}}
\keyword{misc}
