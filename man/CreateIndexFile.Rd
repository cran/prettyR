\name{CreateIndexFile}
\alias{CreateIndexFile}
\title{Write an index file for the current output (htmlize).}
\description{Write an index file for the current HTML output.}
\usage{CreateIndexFile(HTMLbase,HTMLdir,title="R listing")}
\arguments{
 \item{HTMLbase}{The base name for the HTML files.}
 \item{HTMLdir}{The directory in which to write the HTML files.}
 \item{title}{The title for the listing.}
}
\details{
 \samp{CreateIndexFile} opens a new HTML index file. If there is another
 file with the same name, it will be overwritten. This is intentional,
 as the user may want to run \samp{htmlize} repeatedly without generating
 multiple sets of files. It then writes the frameset definition into the
 file and closes it.
}
\value{nil}
\author{Jim Lemon}
\keyword{misc}
