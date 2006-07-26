\name{dstats}
\alias{dstats}
\title{Simple descriptive statistics for numeric variables}
\description{
 calculates mean, variance or standard deviation and n for numeric variables
}
\usage{
 dstats(x,indices=NA,do.sd=FALSE)
}
\arguments{
  \item{x}{data frame, matrix or vector containing numeric variables}
  \item{indices}{which columns of the data frame or matrix to use}
  \item{do.sd}{logical - return standard deviation rather than variance}
}
\details{
 \code{dstats} will accept a vector, matrix or data frame. It calculates the
 mean, variance and valid n for the columns specified by \code{indices}.
 If \code{indices} is not specified, it will be calculated for the 
 columns that return TRUE from \code{as.numeric}. If \code{do.sd}
 is TRUE, it returns standard deviations rather than variances. Valid ns are 
 calculated as the number of observations that are NOT NA.
}
\value{
 A matrix of three rows with the number of columns equal to the number of 
  variables used.
 \item{Mean}{the results of applying the \code{mean} function}
 \item{Variance or SD}{the results of applying the \code{var} function
  or the square root of that if \code{do.sd} is TRUE}
 \item{n}{the number of observations that are NOT NA}
}
\author{Jim Lemon}
\seealso{\code{\link{mean}},\code{\link{var}}}
\examples{
 test.df<-data.frame(A=sample(1:100,20),B=rnorm(20),C=sample(LETTERS,20))
 dstats(test.df)
}
\keyword{misc}
