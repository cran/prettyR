\name{describe.numeric}
\alias{describe.numeric}
\title{
 Description of numeric variables
}
\description{
 Describes a numeric variable.
}
\usage{
 describe.numeric(x,num.desc=c("mean","median","var","sd","valid.n"),
 varname="",vname.space=10,fname.space=10)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{num.desc}{The names of the functions to apply to the vector.}
  \item{varname}{The variable name to display.}
  \item{vname.space}{The width of the variable name display.}
  \item{fname.space}{The width of the descriptive columns.}
}
\details{
 \code{describe.numeric} displays the name of the vector and the results of the
 functions whose names are passed in \code{num.desc}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{describe}}, \code{\link{valid.n}}}
\keyword{misc}
