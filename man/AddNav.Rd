\name{AddNav}
\alias{AddNav}
\title{Add a navigation item}
\description{Add an item to the HTML navigation file.}
\usage{AddNav(Rcommand,navcon,listname,navindex)}
\arguments{
 \item{Rcommand}{The current R command in the script file.}
 \item{navcon}{The connection for writing to the navigation file.}
 \item{listname}{The name of the listing file.}
 \item{navindex}{The number used to generate the name tag.}
}
\details{
 \code{AddNav} adds an entry to the navigation file, generates
 a name tag for that entry and returns it to be inserted into the 
 listing file. If the \R{} command is longer than 20 characters, it
 is truncated to 18 characters and an ellipsis appended.
}
\value{
 The name tag to link the listing to the navigation entry.
}
\author{Jim Lemon}
\keyword{misc}
