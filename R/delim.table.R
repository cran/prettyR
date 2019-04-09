delim.table<-function(x,filename="",delim=",",tabegin="",bor="",eor="\n",
 tablend="",label=deparse(substitute(x)),header=NULL,trailer=NULL,html=FALSE,
 show.rownames=TRUE,leading.delim=FALSE,show.all=FALSE,nsignif=4,
 con,open.con=FALSE) {

 if(html) {
  if(delim == ",") delim="<td>"
  if(tabegin == "") tabegin="<table border=1>\n"
  if(bor == "") bor="<tr><td>"
  if(eor == "\n") eor="</tr>\n"
  if(tablend == "") tablend="</table>\n"
  if(is.null(header)) header="<html><body>\n"
  if(is.null(trailer)) trailer="</body></html>\n"
 }
 if(missing(con)) {
  if(nzchar(filename)) {
   con<-file(filename,"w")
   # only this invocation knows that it has to close con
   open.con<-TRUE
  }
  else con<-""
  if(!is.null(header)) cat(header,"\n",file=con)
 }
 if(is.list(x) && !is.data.frame(x) && length(x) > 1) {
  # break it down into components
  if(!is.null(label)) cat(label,eor,file=con)
  # when recursively calling delim.table, make sure that it doesn't try to open
  # another connection or close the one that is open
  for(component in 1:length(x))
   delim.table(x[[component]],filename="",delim=delim,tabegin=tabegin,bor=bor,
    eor=eor,tablend=tablend,label=names(x[component]),html=FALSE,
    show.rownames=show.rownames,leading.delim=leading.delim,
    show.all=show.all,con=con)
 }
 else {
  xdim<-dim(x)
  if(length(xdim) > 2) stop("delim.table can only process 2D tables")
  if(is.null(xdim)) {
   if(show.all) {
    cat(label,eor,file=con)
    if(is.vector(x)) {
     if(is.expression(x)) cat("Can't print expression",file=con)
     else {
      for(xindex in 1:length(x))
       cat(ifelse(is.numeric(x[xindex]),signif(x[xindex],nsignif),
        x[xindex]),delim,sep="",file=con)
     }
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
   cat(label,"\n",tabegin,"\n",sep="",file=con)
   if(show.rownames) row.names<-rownames(x)
   else row.names<-NULL
   col.names<-names(x)
   if(is.null(col.names)) col.names<-colnames(x)
   if(!is.null(col.names)) {
    if(show.rownames && !is.null(row.names)) {
     if(leading.delim) cat(delim,file=con)
     cat(col.names,sep=delim,file=con)
    }
    cat(eor,file=con)
   }
   for(row in 1:xdim[1]) {
    if(nzchar(bor)) cat(bor,file=con)
    if(show.rownames && !is.null(row.names)) {
     if(leading.delim) cat(delim,file=con)
     cat(row.names[row],file=con)
    }
    nxp<-ifelse(is.na(xdim[2]),length(x),xdim[2])
    for(col in 1:nxp) {
     nextx<-ifelse(is.na(xdim),x[col],x[row,col])
     cat(delim,ifelse(is.numeric(nextx),signif(nextx,nsignif),nextx),
      sep="",file=con)
    }
    cat("\n",file=con)
   }
   cat(eor, file = con)
  }
  cat(tablend,ifelse(html,"",eor),file=con)
 }
 if(open.con) {
  if(!is.null(trailer)) cat(trailer,"\n",file=con)
  close(con)
 }
}
