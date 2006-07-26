\name{integer.frequency}
\alias{integer.frequency}
\title{Calculate frequencies for a vector of integers}
\description{
 Calculates frequencies for a vector of integers
}
\usage{
 integer.frequency(x,bins)
}
\arguments{
  \item{x}{A vector of integers}
  \item{bins}{The values to match for counting.}
}
\details{
 \code{integer.frequency} counts the number of observations in \code{x}
 that match one of the values in \code{bins}.
}
\value{A vector of frequencies the same length as \code{bins}.}
\author{Jim Lemon}
\seealso{\code{\link{freq}}}
\keyword{misc}
