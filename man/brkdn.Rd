\name{brkdn}
\alias{brkdn}
\title{Breakdown of a numeric variable by grouping variable(s)}
\description{
 Calculates means, variances and ns for subgroups of numeric observations and
 displays the results.
}
\usage{
 brkdn(formula,data,maxlevels=10,num.desc=c("mean","var","valid.n"),
  vname.width=NULL,width=10,round.n=2)
}
\arguments{
 \item{formula}{a formula with the variable to be broken down on the left and
  the names of one or more variables defining subgroups on the right}
 \item{data}{the data frame from which to select the variables}
 \item{maxlevels}{the maximum number of levels in any subgroup}
 \item{num.desc}{names of the summary functions to apply to the variable
  on the left side of the formula}
 \item{vname.width}{The number of characters to allow for printing the
  variable name. If NULL, the function will calculate this.}
 \item{width}{The number of characters to allow for each column in the
  summary output.}
 \item{round.n}{The number of decimal places to round the output.}
}
\details{
 \samp{brkdn} will accept a formula referring to columns in a data frame. 
 It calls \samp{describe.numeric} for the calculations and displays a 
 table of results.
}
\value{
 The results of \samp{describe.numeric}, or a multi-level list if more than 
 one grouping variable is specified.}
\author{Jim Lemon}
\seealso{\link{describe.numeric}}
\examples{
 test.df<-data.frame(Age=rnorm(100)+3*10,Sex=sample(c("M","F"),100,TRUE),
  Employ=sample(c("FT","PT","NO"),100,TRUE))
 # add value labels for Employ in alphabetical order so they match
 attr(test.df$Employ,"value.labels")<-c("Full time","None","Part time")
 brkdn(Age~Sex+Employ,test.df)
}
\keyword{misc}
