\name{Mode}
\alias{Mode}
\title{
 Find the modal value
}
\description{
 Finds the modal value of an object (usually a factor).
}
\usage{
 Mode(x,na.rm=FALSE)
}
\arguments{
  \item{x}{An object, usually a factor.}
  \item{na.rm}{A dummy argument to make it compatible with calls to
   \samp{mean}, etc.}
}
\details{
 \samp{Mode} finds the modal value of the object. If there are
 multiple modal values, it returns an appropriate message. If
 \samp{Mode} is called with a continuous variable, it will not in
 general return a sensible answer. It does not attempt to estimate
 the density of the values and return an approximate modal value.
}
\note{
 This is not the same as \samp{mode} that determines the data mode of
 an object.
}
\value{The modal value of the object as a character string.}
\author{Jim Lemon}
\seealso{\link{describe}}
\keyword{misc}
