\name{decimal.align}
\alias{decimal.align}
\title{
 Turn numbers into strings with aligned decimal points
}
\description{
 Formats decimal numbers as strings with aligned decimal points.
}
\usage{
 decimal.align(x,dechar=".",nint=NA,ndec=NA,pad.left=TRUE)
}
\arguments{
 \item{x}{One or more decimal numbers.}
 \item{dechar}{The character used to separate the decimal part of a number.}
 \item{nint}{The number of characters to which the integer part of the
  numbers should be padded.}
 \item{ndec}{The number of characters to which the decimal part of the
  numbers should be padded.}
 \item{pad.left}{Whether the left (integer) side of the numbers should be
  padded as well as the right.}
}
\details{
 \samp{decimal.align} splits the incoming numbers at the decimal point
 and pads the decimal part and optionally the integer part so that when
 the numbers are displayed in a monospaced font the decimal points will
 be aligned. Note that if an integer or a decimal part without an integer
 is passed, the function will insert a zero for the missing part.

 This is useful for displaying or storing aligned columns of decimal
 numbers when the user does not want to pad the decimal part with zeros
 as in the \samp{format} function.
}
\value{
 The original numbers as strings, padded with spaces.
}
\author{Jim Lemon}
\seealso{\link{sprintf}}
\examples{
 x<-c(1,2.3,44.55,666.777)
 decimal.align(x)
}
\keyword{misc}
