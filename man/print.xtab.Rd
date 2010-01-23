\name{print.xtab}
\alias{print.xtab}
\title{Display a 2D crosstabulation}
\description{Displays a 2D crosstabulation with optional chi-squared test,
 odds ratio/relative risk and phi coefficient.}
\usage{
 \method{print}{xtab}(x,col.width=8,or=TRUE,chisq=FALSE,phi=FALSE,...)
}
\arguments{
 \item{x}{The list returned by \samp{calculate.xtab}.}
 \item{col.width}{Width of the columns in the display.}
 \item{or}{whether to calculate the odds ratio and relative risk
  (only for 2x2 tables).}
 \item{chisq}{Whether to call \samp{chisq.test} and display the result.}
 \item{phi}{Whether to calculate and display the phi coefficient
  (only for 2x2 tables).}
 \item{...}{additional arguments passed to \samp{chisq.test}.}
}
\details{
 \samp{print.xtab} displays a crosstabulation in a fairly conventional style with
 row, column and marginal percentages.

 If \samp{or} is \samp{TRUE} and the resulting table is 2x2, the odds ratio will
 be displayed below the table. If the function \samp{logical.names} within
 \samp{print.xtab} finds that the column margin names are one of FALSE/TRUE,
 0/1 or NO/YES in those orders, the risk of the column variable for the
 second level of the row variable relative to the first row variable
 will be displayed as well.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{calculate.xtab}, \link{xtab}}
\keyword{misc}
