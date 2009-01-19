\name{delim.table}
\alias{delim.table}
\title{Format a 2D table}
\description{Format a 2D table with delimiters and other formatting commands}
\usage{
 delim.table(x,filename="",delim=",",tabegin="",bor="",eor="\n",tablend="",
  label=deparse(substitute(x)),header=NULL,trailer=NULL,
  show.rownames=TRUE,leading.delim=TRUE,show.all=FALSE,con,open.con=FALSE)
}
\arguments{
 \item{x}{A list, matrix or data frame that is to be formatted.}
 \item{filename}{Name for a file to which the result will be written.}
 \item{delim}{The delimiter to place between entries in the table(s).}
 \item{tabegin}{Any formatting commands to be placed before the table.}
 \item{bor}{The formatting command for the beginning of a table row.}
 \item{eor}{The formatting command for the end of a table row.}
 \item{tablend}{Any formatting commands to be placed after the table.}
 \item{label}{A label to be displayed before the table.}
 \item{header}{A character string to be written at the beginning of the
  output file.}
 \item{trailer}{A character string to be written at the end of the
  output file.}
 \item{show.rownames}{Whether to display the rownames of a table.}
 \item{leading.delim}{Whether to add an extra delimiter at the beginning of
  the table. See Details.}
 \item{show.all}{Whether to show all the components of a list. The default
  FALSE is to show only those components that look like 2D tables.}
 \item{con}{A connection to which the output will be written. If a filename
  is passed, it will be ignored if \code{con} is not missing.}
 \item{open.con}{A flag for an open connection. This argument is used by
  the function to keep track of connections in recursive calls and should
  not be altered by the user.}
}
\value{
 nil
}
\details{
 \code{delim.table} tries to format its first argument into one or more tables
 that will be displayed in another application. The most common use is to
 produce a CSV style file that can be imported into a spreadsheet. The default
 values for \code{delim} and \code{eor} should be adequate for this, and all the
 user has to do is to supply a filename as in the first example. When a filename
 is provided, the function attempts to open the file, write its output to it and
 close it again. In order to deal with the multilevel lists that are often
 produced by other functions, the function calls itself until it reaches the
 lowest level of the list, where it can successfully format the contents. Thus
 the function only passes the connection, not the filename, in recursive calls.
 If the user passes both a filename and a valid connection, the output will be
 written to the connection and the filename will not be used.

 \code{delim.table} will fail if passed a table with more than two dimensions.
 However, the function will process 2D "slices" of such tables if called with
 one of the \code{apply} family of functions or manually for each slice.

 \code{delim.table} can also be used to format HTML tables as in the second
  example. In principle, any markup language that can produce a table using
  commands that include; commands to begin and end the table, a command to 
  start and end a row, and a command to start a new cell.

 \code{delim.table} is consistent with the default CSV arguments, adding an
 extra comma to the first line if there are row names. The user should set
 \samp{leading.delim} to FALSE for a table without the empty cell at the upper
 left. When \code{delim.table} is used to format tables for \code{htmlize}, it
 should not attempt to open a new connection.
}
\author{Jim Lemon}
\seealso{\code{\link{htmlize}}}
\examples{
 testdf<-data.frame(a=sample(0:1,100,TRUE),b=rnorm(100),c=rnorm(100))
 testglm<-summary(glm(a~b*c,testdf,family="binomial"))
 # produce a CSV file to import into a spreadsheet, just the coefficients
 delim.table(testglm$coefficients,"testglm.csv")
 # now create an HTML file of the three tables in the result
 delim.table(testglm,"testglm.html",delim="<td>",tabegin="<table border=1>",
  bor="<tr><td>",tablend="</table>",header="<html><body>",
  trailer="</body></html>")
 # to clean up, delete the files "testglm.csv" and "testglm.html"
}
\keyword{misc}
