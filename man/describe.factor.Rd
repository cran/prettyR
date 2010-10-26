\name{describe.factor}
\alias{describe.factor}
\title{
 Description of factor variables
}
\description{
 Describes a factor variable.
}
\usage{
 describe.factor(x,varname="",vname.space=10,maxfac=10,show.pc=TRUE,
  horizontal=FALSE)
}
\arguments{
 \item{x}{A factor.}
 \item{varname}{A name for the variable. \samp{describe} always passes
  the name.}
 \item{vname.space}{The width of the variable name display.}
 \item{maxfac}{The maximum number of factors to display.}
 \item{show.pc}{Whether to show the percentages for each level.}
 \item{horizontal}{Whether to display the results across the page (TRUE)
  or down the page (FALSE).}
}
\details{
 \samp{describe.factor} displays the name of the factor, a table of its 
 values with up to \samp{maxfac} entries, the modal value of the factor, the 
 number of valid (not NA) values and the number of values not displayed 
 if it contained more than \samp{maxfac} levels.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{Mode}, \link{valid.n}, \link{table}}
\keyword{misc}
