stretch_df<-function(data,idvar,to.stretch,ordervar=NA,include.ordervar=TRUE) {
 datadim<-dim(data)
 # make up names if there are none
 if(is.null(names(data))) names(data)<-paste("V",1:datadim[2],sep="")
 # get the names of columns not to stretch or drop
 nostretch<-names(data)[!(names(data) %in% c(idvar,to.stretch,ordervar))]
 # get the maximum number of columns for the stretched fields
 maxstretch<-max(table(data[,idvar]))
 # get the indices of the non-stretched fields
 nsindx<-which(!duplicated(data[,idvar]))
 # make the ID field the first column
 IDs<-data[nsindx,idvar]
 newdf<-data.frame(IDs)
 # add the other non-stretched columns
 for(newcol in 1:length(nostretch)) {
  newdf[,newcol+1]<-data[nsindx,nostretch[newcol]]
 }
 start<-length(nostretch)+1
 # if including the order variable(s) in the output
 if(include.ordervar) to.stretch<-c(to.stretch,ordervar)
 # add space for the stretched columns
 for(stretchcol in 1:length(to.stretch)) {
  for(newcol in (start+1):(start+maxstretch)) newdf[[newcol]]<-NA
  start<-start+maxstretch
 }
 nstretch<-length(to.stretch)
 if(is.na(ordervar)) ordervar<-1:maxstretch
 # make column names
 names(newdf)<-c(idvar,nostretch,
  paste(rep(to.stretch,each=maxstretch),
  rep(1:maxstretch,nstretch),sep="_"))
 # step through the IDs
 for(idno in 1:length(IDs)) {
  rows<-which(data[,idvar] == IDs[idno])
  nrows<-length(rows)
  start<-length(nostretch)+1
  # step through the variables to stretch
  for(stretchvar in 1:nstretch) {
   # if there is no order variable, use the existing order
   stretchorder<-order(data[rows,ordervar])
   newdf[idno,(start+1):(start+nrows)]<-
    data[rows,to.stretch[stretchvar]][stretchorder]
   start<-start+maxstretch
  }
 }
 return(newdf)
}
