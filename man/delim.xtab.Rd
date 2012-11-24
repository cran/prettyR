\name{delim.xtab}
\alias{delim.xtab}
\title{Format a crosstabulation}
\description{Format a 2D crosstabulation from xtab}
\usage{
 delim.xtab(x,pct=c("row","column","cell"),coltot=TRUE,rowtot=TRUE,
  ndec=1,delim="\t",interdigitate=TRUE,label=deparse(substitute(x)))
}
\arguments{
 \item{x}{An object of class \samp{xtab}.}
 \item{pct}{Whether and how to calculate percentages.}
 \item{coltot,rowtot}{Whether to add the marginal totals.}
 \item{ndec}{The number of decimal places for percentages.}
 \item{delim}{The delimiter to use between columns. Defaults to TAB.}
 \item{interdigitate}{Whether to place each column of percentages next to
  its row of counts.}
 \item{label}{A label to be displayed before the table.}
}
\value{nil}
\details{
 \samp{delim.xtab} formats a crosstabulation in a manner similar to those
 produced by commercial spreadsheets, with a column of counts followed by a
 column of percentages. If \samp{interdigitate} is FALSE, the percentages will
 be displayed separately. 

 \samp{delim.xtab} will only process one 2D xtab object at a time.

 To format only the counts, set \samp{pct} to NA.

 The intended use of \samp{delim.xtab} is producing tables that can be imported
 into most word processors. If a table is output to
 an HTML or text document formatted with TAB characters, the output can be copied
 and pasted into the word processor document and then converted to a table. 
}
\author{Jim Lemon}
\seealso{\link{xtab}}
\examples{
 alpha1<-sample(LETTERS[1:3],50,TRUE)
 alpha2<-sample(LETTERS[1:2],50,TRUE)
 alphas<-data.frame(alpha1,alpha2)
 alphatab<-xtab(alpha1~alpha2,alphas)
 delim.xtab(alphatab,pct="row",interdigitate=TRUE)
 delim.xtab(alphatab,pct="column",interdigitate=TRUE)
 delim.xtab(alphatab,pct="cell",interdigitate=TRUE)
}
\keyword{misc}
