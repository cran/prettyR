\name{StartList}
\alias{StartList}
\title{Write the header for the HTML listing file (htmlize).}
\description{Initiate the listing file for an R listing in HTML format.}
\usage{StartList(listcon,title="R listing",bgcolor="#dddddd",useCSS=NULL)}
\arguments{
 \item{listcon}{The connection for writing to the listing file.}
 \item{title}{The title and heading for the listing frame.}
 \item{bgcolor}{The background color for the listing frame.}
 \item{useCSS}{Path and filename of a CSS stylesheet.}
}
\details{
 \samp{StartList} sets up the file with the listing
 frame information for the HTML listing.
}
\value{nil}
\author{Jim Lemon}
\keyword{misc}
