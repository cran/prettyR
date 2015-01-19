\name{print.desc}
\alias{print.desc}
\title{Display descriptive stats output}
\description{
 Displays a list of descriptive statistics produced by \samp{describe}.
}
\usage{
 \method{print}{desc}(x,ndec=2,...)
}
\arguments{
  \item{x}{a list of descriptive statistics produced by \samp{describe}}
  \item{ndec}{The number of decimal places to display.}
  \item{...}{additional arguments passed to \samp{print}}
}
\details{
 \samp{print.desc} displays the list of descriptive statistics produced 
 by the \samp{describe} function.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{describe}}
\examples{
 test.df<-data.frame(A=c(sample(1:10,99,TRUE),NA),C=sample(LETTERS,100,TRUE))
 test.desc<-describe(test.df)
 print(test.desc)
}
\keyword{misc}
