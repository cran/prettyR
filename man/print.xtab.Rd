\name{print.xtab}
\alias{print.xtab}
\title{Display a 2D crosstabulation}
\description{Displays a 2D crosstabulation with optional chi-squared test and phi coefficient.}
\usage{
 \method{print}{xtab}(x,col.width=8,chisq=FALSE,phi=FALSE,...)
}
\arguments{
 \item{x}{The list returned by \code{calculate.xtab}.}
 \item{col.width}{Width of the columns in the display.}
 \item{chisq}{Whether to call \code{chisq.test} and display the result.}
 \item{phi}{Whether to calculate and display the phi coefficient (only for 2x2 tables).}
 \item{...}{additional arguments passed to \code{chisq.test}.}
}
\details{
 \code{print.xtab} displays a crosstabulation in a fairly conventional style with row,
 column and marginal percentages.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{calculate.xtab}}, \code{\link{xtab}}}
\keyword{misc}
