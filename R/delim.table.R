delim.table<-function(x,filename="",delim=",",tabegin="",bor="",eor="\n",tablend="",
 label=deparse(substitute(x)),show.all=FALSE,con,open.con=FALSE) {

 if(missing(con)) {
  if(nchar(filename)) {
   con<-file(filename,"w")
   # only this invocation knows that con is open
   open.con<-TRUE
  }
  else con<-""
 }
 if(is.list(x) && length(x) > 1) {
  # break it down into components
  cat(label,eor,file=con)
  # when recursively calling delim.table, make sure that it doesn't try to open
  # another connection or close the one that is open
  for(component in 1:length(x))
   delim.table(x[[component]],filename="",delim=delim,tabegin=tabegin,bor=bor,
    eor=eor,tablend=tablend,label=names(x[component]),show.all=show.all,con=con)
 }
 else {
  xdim<-dim(x)
  if(length(xdim) > 2) stop("delim.table can only process 2D tables")
  if(is.null(xdim)) {
   if(show.all) {
    cat(label,eor,file=con)
    if(is.vector(x)) {
     if(is.expression(x)) cat("Can't print expression",file=con)
     else for(xindex in 1:length(x)) cat(x[xindex],delim,sep="",file=con)
     cat(eor,eor,file=con)
    }
    else {
     options(show.error.messages = FALSE)
     xchar<-try(as.character(x))
     options(show.error.messages = TRUE)
     #if(class(xchar) != "try-error") 
     cat(xchar,eor,file=con)
    }
   }
  }
  else {
   cat(label,eor,tabegin,eor,sep="",file=con)
   row.names<-rownames(x)
   col.names<-names(x)
   if(is.null(col.names)) col.names<-colnames(x)
   if(!is.null(col.names)) {
    if(nchar(bor)) cat(bor,file=con)
    for(column in 1:xdim[2]) cat(delim,col.names[column],sep="",file=con)
    cat(eor,file=con)
   }
   for(row in 1:xdim[1]) {
    if(nchar(bor)) cat(bor,file=con)
    if(!is.null(row.names)) cat(row.names[row],file=con)
   for(column in 1:xdim[2]) cat(delim,x[row,column],sep="",file=con)
    cat(eor,file=con)
   }
   cat(tablend,eor,file=con)
  }
 }
 if(open.con) close(con)
}
