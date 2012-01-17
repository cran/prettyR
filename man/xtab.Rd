\name{xtab}
\alias{xtab}
\title{Crosstabulate variables}
\description{Crosstabulates variables with small numbers of unique values.}
\usage{
 xtab(formula,data,varnames=NULL,or=TRUE,chisq=FALSE,phi=FALSE,html=FALSE,
  bgcol="lightgray")
}
\arguments{
 \item{formula}{a formula containing the variables to be crosstabulated}
 \item{data}{the data frame from which to select the variables}
 \item{varnames}{optional labels for the variables (defaults to \samp{names(data))}}
 \item{or}{whether to calculate the odds ratio (only for 2x2 tables).}
 \item{chisq}{logical - whether to display chi squared test(s) of the table(s)}
 \item{phi}{whether to calculate and display the phi coefficient
  of association - only for 2x2 tables}
 \item{html}{whether to format the resulting table with HTML tags.}
 \item{bgcol}{background color for the table if html=TRUE.}
}
\details{
 \samp{xtab} will accept a formula referring to columns in a data frame or two explicit
 variable names.
 It calls \samp{calculate.xtab} for the calculations and displays one or more tables of
 results by calling \samp{print.xtab}.
 If \samp{html} is TRUE, the resulting table will be formatted with HTML tags.
}
\value{
 The result of \samp{calculate.xtab} if there is only one table to display, 
 otherwise a nested list of tables.}
\author{Jim Lemon}
\seealso{
 \link{table}, \link{calculate.xtab}, \link{print.xtab}
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
 # now have some fun with ridiculously long factor labels
 testxtab<-data.frame(firstbit=sample(c("Ecomaniacs","Redneck rogues"),50,TRUE),
  secondbit=sample(c("Macho bungy jumpers","Wimpy quiche munchers"),50,TRUE))
 # and format the table in HTML and add some tests
 xtab(secondbit~firstbit,testxtab,html=TRUE,chisq=TRUE,phi=TRUE)
}
\keyword{misc}
