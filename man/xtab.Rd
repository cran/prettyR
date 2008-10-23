\name{xtab}
\alias{xtab}
\title{Crosstabulate variables}
\description{Crosstabulates variables with small numbers of unique values.}
\usage{
 xtab(formula,data,varnames=NULL,chisq=FALSE,phi=FALSE)
}
\arguments{
  \item{formula}{a formula containing the variables to be crosstabulated}
  \item{data}{the data frame from which to select the variables}
  \item{varnames}{optional labels for the variables (defaults to \code{names(data))}}
  \item{chisq}{logical - whether to display chi squared test(s) of the table(s)}
  \item{phi}{whether to calculate and display the phi coefficient
   of association - only for 2x2 tables}
}
\details{
 \code{xtab} will accept a formula referring to columns in a data frame or two explicit
 variable names.
 It calls \code{calculate.xtab} for the calculations and displays one or more tables of
 results by calling \code{print.xtab}.
}
\value{
 The result of \code{calculate.xtab} if there is only one table to display, 
 otherwise nil.}
\author{Jim Lemon}
\seealso{
 \code{\link{table}}, \code{\link{calculate.xtab}}, \code{\link{print.xtab}}
}
\examples{
 test.df<-data.frame(sex=sample(c("MALE","FEMALE"),1000,TRUE),
 suburb=sample(1:4,1000,TRUE),social.type=sample(LETTERS[1:4],1000,TRUE))
 xtab(sex~suburb+social.type,test.df,chisq=TRUE)
 # now add some value labels
 attr(test.df$suburb,"value.labels")<-1:4
 names(attr(test.df$suburb,"value.labels"))<-
  c("Upper","Middle","Working","Slum")
 attr(test.df$social.type,"value.labels")<-LETTERS[1:4]
 names(attr(test.df$social.type,"value.labels"))<-
  c("Gregarious","Mixer","Aloof","Hermit")
 xtab(sex~suburb+social.type,test.df)
}
\keyword{misc}
