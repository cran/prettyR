\name{HTMLgraph}
\alias{HTMLgraph}
\title{Create a graphic in HTML output (R2html).}
\description{Creates a graphic file and links it to the HTML output.}
\usage{HTMLgraph(listfile, graphfile = NULL,type = "png",...)}
\arguments{
 \item{listfile}{The name of the HTMLize listing file.}
 \item{graphfile}{The name for the graphic file (see Details).}
 \item{type}{The graphic format in which to write the image.}
 \item{...}{Additional arguments - currently ignored.}
}
\details{
 \code{HTMLgraph} sets up a graphic device to allow an R graphic to be
 written to a file and that file linked to the HTML listing. If no
 filename is passed, a name is constructed from \code{fig} and a number
 that does not match any existing \code{fignnn...} file. Only \code{bmp,
 jpeg} and \code{png} files are currently handled, defaulting to the last.
}
\value{nil}
\author{Phillipe Grosjean}
\keyword{misc}
