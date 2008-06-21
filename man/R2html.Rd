\name{R2html}
\alias{R2html}
\title{Read an R script and write HTML output}
\description{Produces HTML output from an R script.}
\usage{R2html(Rfile,HTMLfile,echo=TRUE,split=FALSE,browse=TRUE,
 title="R listing",bgcolor="#dddddd",...)}
\arguments{
 \item{Rfile}{The R script file from which to read the commands.}
 \item{HTMLfile}{The name for the HTML index file (see Details).}
 \item{echo}{Whether to include ("echo") the commands in the listing.}
 \item{split}{Whether to split the output (see \code{\link{sink}})}.
 \item{browse}{Whether to automatically open the HTML output in the
  default browser when finished.}
 \item{title}{The title of the HTML page and the headings for the
  frames.}
 \item{bgcolor}{The background color for the frames.}
 \item{...}{Additional arguments - currently ignored.}
}
\details{
 \code{R2html} allows the user to produce an HTML listing from
 an existing R script. The script must already run correctly and, if
 there is any graphic output, contain the necessary comments at the 
 end of each graphic command to set up the graphic devices.
 The graphic files will be linked to the HTML listing page so
 that they should be interleaved with text output and commands.
 
 Three files will be output. The first will be named \code{HTMLfile}
 which must be a valid filename with the extension \code{.html}.
 This file is the "index" file that sets up the HTML frameset.
 The second file will be named by concatenating \code{HTMLfile}
 without its extension and \code{_nav.html}. Its contents will be 
 dispayed at the left side of the HTML output as a "navigation" list 
 using the commands as names. The third file is named by concatentating
 \code{HTMLfile} without its extension and \code{_list.html}. This
 contains the program listing. All three files will be written in 
 whatever directory is specified by the path to \code{HTMLfile}. If this
 is missing, everything will be written in the current R directory.
 
 Commands that create or alter connections, such as \code{sink} are
 "forbidden", not evaluated and marked as comments in the listing.
 This prevents such commands from altering the connections necessary
 to write the HTML files.
 
 To include graphic output in the HTML file, place a comment at the end of
 any function that produces a graphic like this \code{#--FIG:filename.png--}
 and the appropriate graphic device is automatically set up. The filename
 may be left out, in which case a name will be generated.
}
\value{nil}
\author{Philippe Grosjean}
\examples{
 rcon<-file("testR2html.R","w")
 cat("test.df<-data.frame(a=factor(sample(LETTERS[1:4],100,TRUE)),\n",
  file=rcon)
 cat(" b=sample(1:4,100,TRUE),c=rnorm(100),d=rnorm(100))\n",file=rcon)
 cat("describe(test.df)\n",file=rcon)
 cat("print(freq(test.df$a))\n",file=rcon)
 cat("xtab(a~b,test.df)\n",file=rcon)
 cat("brkdn(c~b,test.df)\n",file=rcon)
 cat("hist(test.df$b)#--FIG:hista.png--\n",file=rcon)
 cat("plot(test.df$c,test.df$d)#--FIG:plotcd.png--\n",file=rcon)
 close(rcon)
 # R2html("testR2html.R", "testR2html.html")
 # if you want to see the output, use the following line
 # system(paste(options("browser")," file:",getwd(),"/testR2html.html",sep="",collapse=""))
 # to clean up, use the following line
 # system("rm testR2html.R testR2html.html testR2html_nav.html testR2html_list.html hista.png plotcd.png")
}
\keyword{misc}
