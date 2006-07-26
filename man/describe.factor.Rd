\name{describe.factor}
\alias{describe.factor}
\title{
 Description of factor variables
}
\description{
 Describes a factor variable.
}
\usage{
 describe.factor(x,varname="",vname.space=10)
}
\arguments{
 \item{x}{A factor.}
 \item{varname}{A name for the variable. \code{describe} always passes
  the name.}
 \item{vname.space}{The width of the variable name display.}
}
\details{
 \code{describe.factor} displays the name of the factor, a table of its 
 values with up to eight entries, the modal value of the factor, the 
 number of valid (not NA) values and the number of values not displayed 
 if it contained more than eight levels.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{Mode}}, \code{\link{valid.n}}, \code{\link{table}}}
\keyword{misc}
