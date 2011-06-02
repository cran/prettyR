\name{calculate.xtab}
\alias{calculate.xtab}
\title{Calculate a crosstabulation}
\description{Calculates the marginal totals and names for a crosstabulation.}
\usage{
 calculate.xtab(v1,v2,varnames=NULL)
}
\arguments{
  \item{v1,v2}{The variables to be crosstabulated.}
  \item{varnames}{Labels for the variables (defaults to \samp{names(data))}}
}
\details{
 \samp{calculate.xtab} calls \samp{table} for the base table, calculates
 the marginal totals and returns a list with these and the names of the
 variables that will be used by \samp{print.xtab}.}
\value{
 A list containing the value of \samp{table}, the row and column marginals
 and the names of the variables.}
\author{Jim Lemon}
\seealso{\link{table}, \link{print.xtab}}
\keyword{misc}
