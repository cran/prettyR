\name{toNA}
\alias{toNA}
\title{Set specified values in an object to NA}
\description{Sets all specified values in an object to NA.}
\usage{toNA(x,values=NA)}
\arguments{
 \item{x}{A vector, matrix or data frame (max 2D).}
 \item{values}{One or more values that are to be set to NA.}
}
\details{
 \samp{toNA} sets all entries in an object in values to NA. Useful for
 converting various missing value samps to NA.
}
\value{The object with NAs replacing all specified values.}
\author{Jim Lemon}
\seealso{\link{\%in\%}}
\keyword{misc}
