\name{htmlize}
\alias{htmlize}
\title{Read an R script and write HTML output}
\description{Produces HTML output from an R script.}
\usage{htmlize(Rfile,HTMLbase,HTMLdir,title,
 bgcolor="#dddddd",echo=FALSE,do.nav=FALSE,useCSS=NULL,...)}
\arguments{
 \item{Rfile}{The R script file from which to read the commands.}
 \item{HTMLbase}{The base name for the HTML files (see Details).}
 \item{HTMLdir}{The directory in which to write the HTML output.}
 \item{title}{The title of the HTML page and the headings for the
  frames. See Details for including the title in the R script.}
 \item{bgcolor}{The background color for the frames.}
 \item{echo}{Whether to include ("echo") the commands in the listing.}
 \item{do.nav}{Whether to have a navigation window.}
 \item{useCSS}{The name of a CSS stylesheet that will define the
  appearance of components of the HTML display. If this is not NULL,
  the CSS file should exist.}
 \item{...}{Additional arguments - currently ignored.}
}
\details{
 \samp{htmlize} allows the user to produce a basic HTML listing from
 an existing R script. The script must already run correctly with
 \samp{source}. If the first line of the R script is a comment starting
 with \samp{#title~} and the \samp{title} argument is missing, the rest
 of the first line will be used as the title of the HTML output.
 
 If there is any graphic output, the script must contain the necessary
 commands to set up the graphic devices. Note that only TIFF, GIF, BMP,
 JPEG and PNG graphic images are generally viewable in HTML browsers.
 The last two are probably the most reliable, but see their help pages
 for more details. The graphic files will be linked to the HTML listing
 page so that they should be interleaved with text output and commands.
 
 If \samp{do.nav} is TRUE, three files will be output. The first will
 be named \samp{HTMLbase.html}, where \samp{HTMLbase} is whatever string
 has been passed as that argument.
 If that argument is missing, the function will attempt to munge the
 \samp{Rfile} argument into a base name. This file is an "index" file
 that sets up the HTML frameset. The second file will be named
 \samp{HTMLbase_nav.html} and will be dispayed at the left side of the HTML
 output as a "navigation" list using the commands as names. Commands
 longer than 20 characters will be truncated. The third file, named
 \samp{HTMLbase_list.html}, contains the program listing. All three 
 files will be written in \samp{HTMLdir}. If this is missing, the path of
 \samp{Rfile} will be used.
 
 If \samp{do.nav} is FALSE, only one file will be written. It will have
 the same content as the \samp{HTMLbase_list.html} file except without
 the name tags for navigation and it will be named \samp{HTMLbase.html}.
 
 Commands that create or alter connections, such as \samp{sink} are
 "forbidden", not evaluated and marked as comments in the listing.
 This prevents such commands from altering the connections necessary
 to write the HTML files.
 
 If there is a function defined in the R script, \samp{htmlize} will run, but
 not write any output after the function definition. This has to do with the
 way that \samp{htmlize} reads command lines from the script file. This is a
 bug, so watch this space for a solution.
 
 The ability to nominate a CSS stylesheet allows the user to customize the
 appearance of the HTML output. The most likely use of the \samp{useCSS}
 argument is for the user to specify whatever aspects of the HTML display
 are to be different from the default browser values in a stylesheet.
}
\note{
 As of version 1.1, both \samp{echo} and \samp{do.nav} are FALSE by default,
 as these defaults seem to be more popular.
 
 The major differences between \samp{htmlize} and \samp{R2html} are:
 \samp{htmlize} will run with a minimum of one argument (\samp{Rfile})
 and produces very basic HTML output. It requires the graphic devices to be
 set up as commands in the R script.
 
 \samp{R2html} does not require commands for graphic devices, just 
 comments at the end of any line that requires graphic output to the HTML
 file. It color codes commands and output and is more careful about the 
 content of lines it writes.
}
\value{nil}
\author{Jim Lemon with improvements by Phillipe Grosjean}
\examples{
 rcon<-file("test.R","w")
 cat("#title~This is the title\n")
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
 # system("rm test.R test.html hista.png plotcd.png")
}
\keyword{misc}
