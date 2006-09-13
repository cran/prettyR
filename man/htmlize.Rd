\name{htmlize}
\alias{htmlize}
\title{Read an R script and write HTML output}
\description{Produces HTML output from an R script.}
\usage{htmlize(Rfile,HTMLbase,HTMLdir,title,
 bgcolor="#dddddd",echo=TRUE,do.nav=TRUE,...)}
\arguments{
 \item{Rfile}{The R script file from which to read the commands.}
 \item{HTMLbase}{The base name for the HTML files (see Details).}
 \item{HTMLdir}{The directory in which to write the HTML output.}
 \item{title}{The title of the HTML page and the headings for the
  frames.}
 \item{bgcolor}{The background color for the frames.}
 \item{echo}{Whether to include ("echo") the commands in the listing.}
 \item{do.nav}{Whether to have a navigation window.}
 \item{...}{Additional arguments - currently ignored.}
}
\details{
 \code{htmlize} allows the user to produce a basic HTML listing from
 an existing R script. The script must already run correctly with
 \code{source}.
 
 If there is any graphic output, the script must contain the necessary
 commands to set up the graphic devices. Note that only TIFF, GIF, BMP,
 JPEG and PNG graphic images are generally viewable in HTML browsers.
 The last two are probably the most reliable, but see their help pages
 for more details. The graphic files will be linked to the HTML listing
 page so that they should be interleaved with text output and commands.
 
 If \code{do.nav} is TRUE, three files will be output. The first will
 be named \code{HTMLbase.html}, where \code{HTMLbase} is whatever string
 has been passed as that argument.
 If that argument is missing, the function will attempt to munge the
 \code{Rfile} argument into a base name. This file is an "index" file
 that sets up the HTML frameset. The second file will be named
 \code{HTMLbase_nav.html} and will be dispayed at the left side of the HTML
 output as a "navigation" list using the commands as names. Commands
 longer than 20 characters will be truncated. The third file, named
 \code{HTMLbase_list.html}, contains the program listing. All three 
 files will be written in \code{HTMLdir}. If this is missing, the path of
 \code{Rfile} will be used.
 
 If \code{do.nav} is FALSE, only one file will be written. It will have
 the same content as the \code{HTMLbase_list.html} file except without
 the name tags for navigation and it will be named \code{HTMLbase.html}.
 
 Commands that create or alter connections, such as \code{sink} are
 "forbidden", not evaluated and marked as comments in the listing.
 This prevents such commands from altering the connections necessary
 to write the HTML files. 
}
\note{
 The major differences between \code{htmlize} and \code{R2html} are:
 \code{htmlize} will run with a minimum of one argument (\code{Rfile})
 and produces very basic HTML output. It requires the graphic devices to be
 set up as commands in the R script.
 
 \code{R2html} does not require commands for graphic devices, just 
 comments at the end of any line that requires graphic output to the HTML
 file. It color codes commands and output and is more careful about the 
 content of lines it writes.
}
\value{nil}
\author{Jim Lemon with improvements by Phillipe Grosjean}
\examples{
 rcon<-file("test.R","w")
 cat("test.df<-data.frame(a=factor(sample(LETTERS[1:4],100,TRUE)),\n",
  file=rcon)
 cat(" b=sample(1:4,100,TRUE),c=rnorm(100),d=rnorm(100))\n",file=rcon)
 cat("describe(test.df)\n",file=rcon)
 cat("print(freq(test.df$a))\n",file=rcon)
 cat("xtab(a~b,test.df)\n",file=rcon)
 cat("brkdn(c~b,test.df)\n",file=rcon)
 cat("png(\"hista.png\")\nhist(test.df$b)\ndev.off()\n",file=rcon)
 cat("png(\"plotcd.png\")\nplot(test.df$c,test.df$d)\ndev.off()\n",file=rcon)
 close(rcon)
 # call htmlize with minimal arguments
 htmlize("test.R")
 # if you want to see the output, use the following line
 # system(paste(options("browser")," file:",getwd(),"/test.html",sep="",collapse=""))
 # to clean up, use the following line
 # system("rm test.R test.html test_nav.html test_list.html hista.png plotcd.png")
}
\keyword{misc}
