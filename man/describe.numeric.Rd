\name{describe.numeric}
\alias{describe.numeric}
\title{
 Description of numeric variables
}
\description{
 Describes a numeric variable.
}
\usage{
 describe.numeric(x,num.desc=c("mean","median","var","sd","valid.n"),
 varname="",vname.space=10,fname.space=10)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{num.desc}{The names of the functions to apply to the vector.}
  \item{varname}{The variable name to display.}
  \item{vname.space}{The width of the variable name display.}
  \item{fname.space}{The width of the descriptive columns.}
}
\details{
 \samp{describe.numeric} displays the name of the vector and the
 results of the functions whose names are passed in \samp{num.desc}.
 Note that any functions that are called by \samp{describe.numeric}
 must have an \samp{na.rm} argument, even if it is a dummy.
}
\value{
 The vector of values returned from the functions in \samp{num.desc}.
}
\author{Jim Lemon}
\seealso{\link{describe}, \link{valid.n}}
\examples{
 x<-rnorm(100)
 # include a function that calculates the "smoothness" of a vector
 # of numbers by calculating the mean of the absolute difference
 # between each successive value - all values equal would be 0
 smoothness<-function(x,na.rm=TRUE) {
  if(na.rm) x<-x[!is.na(x)]
  xspan<-diff(range(x))
  return(mean(abs(diff(x))/xspan,na.rm=TRUE))
 }
 describe(x,num.desc=c("mean","median","smoothness"),xname="x")
 # now sort the values to make the vector "smoother"
 describe(sort(x),num.desc=c("mean","median","smoothness"),xname="x")
}
\keyword{misc}
