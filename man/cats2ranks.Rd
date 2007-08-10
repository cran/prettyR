\name{cats2ranks}
\alias{cats2ranks}
\title{Ordered option selection to mean ranks}
\description{Convert ordered option selections to mean ranks}
\usage{
 cats2ranks(x,cats=NULL)
}
\arguments{
 \item{x}{A matrix or data frame of numeric values that represent ordered
  choices of options. Rows are considered to represent cases or respondents
  and columns are the data entry points, beginning with the highest ranking
  (usually something like "Most important") and descending.}
 \item{cats}{The range of numbers that represent options. The default is the
  vector of unique entries in \code{x}.}
}
\value{
 A list with three components:
 \item{cats}{The vector of options as passed or calculated.}
 \item{ranksum}{The sum of ranks for each option.}
 \item{rankcount}{The number of times each option was selected.}
}
\details{
 \code{cats2ranks} converts ordered option selections to mean ranks. It is
 useful in the situation where a respondent is asked to select one of a number
 of options as the most important, another as the second most important, and
 so on. It counts the number of times each option code appears in each column
 and calculates the mean ranking of options. It is expected that there will be
 fewer selections available than there are options, thus creating the opportunity
 for biased rankings. This can occur when one or more options are not commonly
 chosen, but are given extreme (usually high) ranks when they are. The function
 calculates the mean of unallocated ranks and assigns this to all options not
 chosen by each respondent, correcting for this bias.

 \code{cats2ranks} is especially useful when respondents do not select the same
 number of options. The mean of unallocated ranks is calculated for each
 respondent so that only explicitly selected options are entered into the
 calculation of mean ranks.
}
\author{Jim Lemon}
\seealso{\code{\link{rank}}}
\examples{
 # first a standard 1:m option selection
 opchoice<-matrix(NA,nrow=40,ncol=5)
 for(i in 1:40) opchoice[i,]<-sample(1:10,5)
 cats2ranks(opchoice)
 # now a messy character choice with missing values
 opchoice<-matrix(NA,nrow=40,ncol=5)
 for(i in 1:40) {
  nchoices<-sample(3:5,1)
  opchoice[i,1:nchoices]<-sample(LETTERS[1:10],nchoices)
 }
 cats2ranks(opchoice)
}
\keyword{misc}
