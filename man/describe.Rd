\name{describe}
\alias{describe}
\title{Description of variables}
\description{Describes a vector or the columns in a matrix or data frame.}
\usage{
 describe(x,num.desc=c("mean","median","var","sd","valid.n"),xname=NA,
  maxfac=10,show.pc=TRUE,horizontal=FALSE)
}
\arguments{
 \item{x}{A vector, matrix or data frame.}
 \item{num.desc}{The names of the functions to apply to numeric data.}
 \item{xname}{A name for the object \samp{x}, mostly where this would be
  a very long string describing its structure (e.g. if it was extracted
  by name from a data frame).}
 \item{maxfac}{The maximum number of factors for \samp{describe.factor} to
  display.}
 \item{show.pc}{Whether to show percentages of factor and logical values.}
 \item{horizontal}{Whether to display the results of \samp{describe.factor}
  across the page (TRUE) or down the page (FALSE).}
}
\details{
 \samp{describe} displays a table of descriptive statistics for numeric and
 factor variables in the object \samp{x}. The summary measures for numeric
 variables can easily be altered with the argument \samp{num.desc}. Pass a
 character vector with the names of the desired summary measures and these
 will be displayed at the top of the numeric block with their results beneath
 them. If quantiles are desired, the user will have to write wrapper functions
 that call \samp{quantile} with the appropriate type or probabilities and use
 the names of the wrapper functions in \samp{num.desc}. Remember that any
 function called by \samp{describe} must have an \samp{na.rm} argument.
}
\value{
 A list with three components:
 \item{Numeric}{A matrix of the values returned from \samp{describe.numeric}
  for each column described.}
 \item{Factor}{A matrix of the modal value and number of missing values for
  each column described.}
 \item{Logical}{A matrix of the counts of FALSE, TRUE and missing values for
  each column described.}
}
\author{Jim Lemon}
\seealso{\link{Mode}, \link{valid.n}, \link{describe.numeric},
 \link{describe.factor}}
\examples{
 sample.df<-data.frame(sex=sample(c("MALE","FEMALE"),100,TRUE),
  income=(rnorm(100)+2.5)*20000,suburb=factor(sample(1:4,100,TRUE)))
 # include the maximum values
 describe(sample.df,num.desc=c("mean","median","max","var","sd","valid.n"))
 # make up a function
 roughness<-function(x,na.rm=TRUE) {
  if(na.rm) x<-x[!is.na(x)]
  xspan<-diff(range(x))
  return(mean(abs(diff(x))/xspan,na.rm=TRUE))
 }
 # now use it
 describe(sample.df$income,num.desc="roughness",xname="income")
}
\keyword{misc}
