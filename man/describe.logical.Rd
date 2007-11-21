\name{describe.logical}
\alias{describe.logical}
\title{
 Description of logical variables
}
\description{
 Describes a logical variable.
}
\usage{
 describe.logical(x,varname="",vname.space=10,show.pc=TRUE)
}
\arguments{
 \item{x}{A logical.}
 \item{varname}{An optional name for the variable. \code{describe}
  always passes the name of the variable.}
 \item{vname.space}{The width of the variable name display.}
 \item{show.pc}{Whether to show the percentage of TRUE values.}
}
\details{
 \code{describe.logical} displays the name of the logical, a table 
 of counts of its two values (TRUE, FALSE), optionally the percentage
 of TRUE values and the number of NAs.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{table}}}
\keyword{misc}
