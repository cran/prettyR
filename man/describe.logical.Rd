\name{describe.logical}
\alias{describe.logical}
\title{
 Description of logical variables
}
\description{
 Describes a logical variable.
}
\usage{
 describe.logical(x,varname="")
}
\arguments{
 \item{x}{A logical.}
 \item{varname}{An optional name for the variable. \samp{describe}
  always passes the name of the variable.}
}
\details{
 \samp{describe.logical} displays the name of the logical, a table 
 of counts of its two values (TRUE, FALSE) and the percentage
 of TRUE values.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{table}}
\keyword{misc}
