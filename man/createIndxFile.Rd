\name{createIndxFile}
\alias{createIndxFile}
\title{Write an index file for the current output (R2html).}
\description{Write an index file for the current R2html output.}
\usage{createIndxFile(HTMLfile,navfile,listfile,title="R listing")}
\arguments{
 \item{HTMLfile}{The file name for the HTML files.}
 \item{navfile}{The name for the HTML navigation frame file.}
 \item{listfile}{The name for the HTML listing file.}
 \item{title}{The title for the listing.}
}
\details{
 \code{createIndxFile} opens a new HTML index file. If there is another
 file with the same name, it will be overwritten. This is intentional,
 as the user may want to run \code{R2html} repeatedly without generating
 multiple sets of files. It then writes the frameset definition into the
 file and closes it.
}
\value{nil}
\author{Philippe Grosjean}
\keyword{misc}
