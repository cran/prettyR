\name{rep_n_stack}
\alias{rep_n_stack}
\title{
 Replicate and stack columns of a data frame
}
\description{
 Reshape a data frame by stacking two or more columns into one and adding a
 factor, while replicating the remaining columns and stacking them to match
 the number of rows
}
\usage{
 rep_n_stack(data,to.stack,stack.names=NULL)
}
\arguments{
 \item{data}{A data frame.}
 \item{to.stack}{Which columns are to be stacked together (see Details).}
 \item{stack.names}{Names for the new factor and stacked column.}
}
\details{
 \samp{rep_n_stack} takes two or more specified columns in a data frame and
 "stacks" them into a single column. It also creates a new factor composed of
 the replicated names of the columns that identifies from which column each
 value came. The remaining columns in the data frame are replicated to match
 the new number of rows.

 If \samp{to.stack} is a matrix of names or column numbers, \samp{rep_n_stack}
 will stack each row into two new columns, allowing multiple related sets of
 values to be stacked in one operation. 

 A matrix or data frame of values can now be stacked so that the values can be
 displayed by a function like \samp{barNest} in the \pkg{plotrix} package.
}
\note{
 \samp{rep_n_stack} only does what other reshaping functions can do, but may
 be more easy to understand.
}
\value{The reshaped data frame.}
\author{Jim Lemon}
\seealso{\link{reshape}}
\examples{
 wide.data<-data.frame(ID=1:10,Glup=sample(c("Montic","Subtic"),10,TRUE),
  Flimit1=runif(10,1,2),Flimit2=runif(10,1.5,2.5),Flimit3=runif(10,1.2,3),
  Glimit1=rnorm(10,mean=5),Glimit2=rnorm(10,mean=4),Glimit3=rnorm(10,mean=4.5))
 # first just stack one set of related measures
 rep_n_stack(wide.data[,1:5],to.stack=c("Flimit1","Flimit2","Flimit3"))
 # now stack two sets of related measures and pass names for the stacks
 rep_n_stack(wide.data,to.stack=matrix(3:8,nrow=2,byrow=TRUE),
  stack.names=c("Limit_F","Value_F","Limit_G","Value_G"))
 # finally stack a matrix of means into a single column with the
 # row and column names becoming "factor" variables
 meanmat<-matrix(runif(16,10,20),nrow=4)
 rownames(meanmat)<-c("Plunderers","Storers","Refusers","Jokers")
 colnames(meanmat)<-c("Week1","Week2","Week3","Week4")
 rep_n_stack(meanmat,to.stack=1:4,
  stack.names=c("Returns","Occasion","Strategy"))
}
\keyword{misc}
