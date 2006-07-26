\name{describe.logical}
\alias{describe.logical}
\title{
 Description of logical variables
}
\description{
 Describes a logical variable.
}
\usage{
 describe.logical(x,varname="",vname.space=10)
}
\arguments{
 \item{x}{A logical.}
 \item{varname}{An optional name for the variable. \code{describe}
  always passes the name of the variable.}
 \item{vname.space}{The width of the variable name display.}
}
\details{
 \code{describe.logical} displays the name of the logical, and a table 
 of counts of its two values (TRUE, FALSE) and NAs.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{table}}}
\keyword{misc}
