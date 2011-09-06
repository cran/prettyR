\name{add.value.labels}
\alias{add.value.labels}
\title{Add value labels}
\description{Adds value labels to a variable.}
\usage{
 add.value.labels(x,value.labels)
}
\arguments{
  \item{x}{The variable to add the labels.}
  \item{value.labels}{The labels.}
}
\details{
 \samp{add.value.labels} adds value labels like those from an SPSS .sav file.
 It makes it a bit easier to stick on value labels that have been lost or were
 not there in the first place.}
\value{
 The variable with the labels added.}
\author{Jim Lemon}
\examples{
 fgh<-data.frame(sex=sample(1:2,20,TRUE),viviality=sample(1:3,20,TRUE))
 fgh$sex<-add.value.labels(fgh$sex,c("Female","Male"))
 fgh$viviality<-add.value.labels(fgh$viviality,c("Alive","Dead","Zombie"))
}
\keyword{misc}
