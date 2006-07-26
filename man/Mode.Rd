\name{Mode}
\alias{Mode}
\title{
 Find the modal value
}
\description{
 Finds the modal value of an object (usually a factor).
}
\usage{
 Mode(x,na.rm)
}
\arguments{
  \item{x}{An object, usually a factor.}
  \item{na.rm}{A dummy argument to make it compatible with calls to \code{mean}, etc.}
}
\details{
 \code{Mode} finds the modal value of the object. If there are multiple modal
 values, it returns an appropriate message.
}
\note{
 This is not the same as \code{mode} that determines the data mode of an object.
}
\value{The modal value of the object as character.}
\author{Jim Lemon}
\seealso{\code{\link{describe}}}
\keyword{misc}
