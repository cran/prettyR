\name{describe.factor}
\alias{describe.factor}
\title{
 Description of factor variables
}
\description{
 Describes a factor variable.
}
\usage{
 describe.factor(x,varname="",horizontal=FALSE,decr.order=TRUE)
}
\arguments{
 \item{x}{A factor.}
 \item{varname}{A name for the variable. \samp{describe} always passes
  the name.}
 \item{horizontal}{Whether to display the results across the page (TRUE)
  or down the page (FALSE).}
 \item{decr.order}{Whether to order the rows by decreasing frequency.}
}
\details{
 \samp{describe.factor} displays the name of the factor, a table of
 its values, the modal value of the factor and the number of valid
 (not NA) values.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{Mode}, \link{valid.n}, \link{table}}
\keyword{misc}
