\name{stretch_df}
\alias{stretch_df}
\title{
 Reshape a data frame from "long" to "wide" format
}
\description{
 Reshape a data frame by reducing the multiple rows of repeated variables to
 a single row for each instance (usually a "case" or object) and stretching
 out the variables that are not repeated within each case.
}
\usage{
 stretch_df(data,idvar,to.stretch,ordervar,include.ordervar=TRUE)
}
\arguments{
 \item{data}{A data frame.}
 \item{idvar}{A variable that identifies instances (cases or objects).}
 \item{to.stretch}{Which variables are to be stretched out in the single row.}
 \item{ordervar}{Variable that gives the order of the stretched variables.}
 \item{include.ordervar}{Include the ordering variable in the output.}
}
\details{
 \samp{stretch_df} takes a data frame in which at least some instances have
 multiple rows and reshapes it into a "wide" format with one row per instance.
 The variable passed as \samp{idvar} distinguishes the instances, and will be
 the first column in the new data frame. All other variables in the data frame
 except those named in \samp{to.stretch} and \samp{ordervar} will follow
 \samp{idvar}.
 The variables named in \samp{to.stretch} will follow the variables that are
 not repeated in the initial data frame, along with the order variable if
 \samp{include.ordervar} is TRUE.
}
\note{
 \samp{stretch_df} mostly does what other reshaping functions can do, but may
 be more easy to understand. It will stretch multiple variables, something that
 some reshaping functions will not do.
}
\value{The reshaped data frame.}
\author{Jim Lemon}
\seealso{\link{reshape}}
\examples{
 # create a data frame with two repeated variables
 longdf<-data.frame(ID=c(rep(111,3),rep(222,4),rep(333,6),rep(444,3)),
  name=c(rep("Joe",3),rep("Bob",4),rep("Sue",6),rep("Bea",3)),
  score1=sample(1:10,16,TRUE),score2=sample(0:100,16),
  scoreorder=c(1,2,3,4,3,2,1,4,6,3,5,1,2,1,2,3))
 stretch_df(longdf,"ID",c("score1","score2"),"scoreorder")
}
\keyword{misc}
