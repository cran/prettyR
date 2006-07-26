\name{valid.n}
\alias{valid.n}
\title{
 Find the number of valid (not NA) values
}
\description{
 Finds the number of valid (not NA) values in an object.
}
\usage{
 valid.n(x,na.rm)
}
\arguments{
 \item{x}{An object.}
 \item{na.rm}{A dummy argument to make it compatible with calls to \code{mean}, etc.}
}
\details{
 \code{valid.n} finds the number of valid values of the object.
}
\value{The number of valid values of the object.}
\author{Jim Lemon}
\seealso{\code{\link{describe}}}
\keyword{misc}
