\name{addxtabs}
\alias{addxtabs}
\title{
 Add the cells of two or more xtab objects
}
\description{
 Adds the counts in corresponding cells of xtab objects
}
\usage{
 addxtabs(x)
}
\arguments{
 \item{x}{A list containing two or more return values from \samp{xtab}.}
}
\details{
 \samp{addxtabs} adds the counts in the cells of two or more \samp{xtab} objects
 by matching the row and column names. All of the \samp{xtab} objects in the list
 will usually contain counts of the same response options, although some options
 may be missing in some tables. This often occurs when respondents do not have to
 complete all of the responses (e.g. "for up to three occasions ...").

 This function facilitates calculating the total counts in crosstabulations that
 arise from multiple responses for the options. See the example.
}
\value{The number of valid values or the length of the object.}
\author{Jim Lemon}
\seealso{\link{xtab}}
\examples{
 # Assume that respondents are asked to record the location and quantity for
 # three occasions of drinking, and for each occasion the fields are named
 # "drinks" and "loc"
 drinkloc<-data.frame(drinks1=sample(c("1-2","3-5","6+"),100,TRUE),
  loc1=sample(c("Meal at home","Restaurant","Party","Pub"),100,TRUE),
  drinks2=sample(c("1-2","3-5","6+"),100,TRUE),
  loc2=sample(c("Meal at home","Restaurant","Party","Pub"),100,TRUE),
  drinks3=sample(c("1-2","3-5"),100,TRUE),
  loc3=sample(c("Meal at home","Restaurant","Party"),100,TRUE))
 # notice how two options have been left out in drink3 and loc3
 # create the list of xtab objects
 dltablist<-list()
 dltablist[[1]]<-xtab(loc1~drinks1,drinkloc)
 dltablist[[2]]<-xtab(loc2~drinks2,drinkloc)
 dltablist[[3]]<-xtab(loc3~drinks3,drinkloc)
 # now sum the corresponding cells
 addxtabs(dltablist)
}
\keyword{misc}
