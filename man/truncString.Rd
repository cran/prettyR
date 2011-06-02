\name{truncString}
\alias{truncString}
\title{
 Truncate strings and add ellipses if a string is truncated.
}
\usage{
 truncString(x,maxlen=20,justify="left")
}
\arguments{
 \item{x}{An object.}
 \item{maxlen}{The maximum length of the returned string(s).}
 \item{justify}{Justification for the new strings.}
}
\description{
 Truncates one or more strings to a specified length, adding an ellipsis (...)
 to those strings that have been truncated. The justification of the strings can
 be controlled by the \samp{justify} argument as in \link{format}, which does
 the padding and justification.
}
\value{The string(s) passed as \samp{x} now with a maximum length of \samp{maxlen}.}
\author{Jim Lemon}
\seealso{\link{substr}}
\keyword{misc}
