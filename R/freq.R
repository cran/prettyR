# freq calculates a simple frequency table for a vector, or steps through
# the columns of a data frame or matrix and returns a list of the frequency
# table(s).

freq<-function(x,variable.labels=NULL,display.na=TRUE,levels=NULL) {
 
 if(missing(x))
  stop("A vector, dataframe or matrix must be supplied")
 xdim<-dim(x)
 # get the variable label here or it might be clobbered
 if(is.null(xdim)) {
  if(is.null(variable.labels))
   variable.labels<-deparse(substitute(x))
  x<-list(x)
  nfreq<-1
 }
 else {
  nfreq<-xdim[2]
  if(is.matrix(x))
  x<-as.data.frame(x)
  if(is.null(variable.labels))
   variable.labels<-names(x)
  if(is.null(variable.labels))
   variable.labels<-paste("V",1:xdim[2],sep="",collapse="")
 }
 freq.list<-rep(list(0),nfreq)
 for(i in 1:nfreq) {
  # see if there are any NAs and if they should be displayed
  if(display.na) nna<-sum(is.na(x[[i]]))
  else nna<-0
  # tabulate barfs with NAs
  xt<-na.omit(x[[i]])
  if(is.null(levels)) levels<-unique(xt)
  if(is.numeric(x[[i]])) xt<-factor(xt,levels=levels)
  freqs<-tabulate(xt)
  categories<-levels(xt)
  # if NAs present, tack on a label
  if(nna) categories<-c(categories,"NA")
  # tack on the NA count
  if(nna) freqs<-c(freqs,nna)
  names(freqs)<-categories
  freq.list[[i]]<-freqs
 }
 names(freq.list)<-variable.labels
 class(freq.list)<-"freq"
 return(freq.list)
}

print.freq<-function(x,show.pc=TRUE,...) {
 nfreq<-length(x)
 variable.labels<-names(x)
 for(i in 1:nfreq) {
  categories<-names(x[[i]])
  maxchar<-max(c(nchar(categories),4))
  cat("\nFrequencies for",variable.labels[i],"\n")
  cat("    ",formatC(ifelse(categories=="","Missing",categories),width=maxchar),
   "\n")
  cat("    ",formatC(as.character(x[[i]]),width=maxchar),"\n")
  if(show.pc) {
   percentages<-round(100*x[[i]]/sum(x[[i]]),1)
   cat("%   ",formatC(as.character(percentages),width=maxchar),"\n")
   if(any(names(x[[i]]) == "NA")) {
    xlen<-length(x[[i]])
    # get the number of NAs
    nna<-x[[i]][xlen]
    percentages<-round(100*x[[i]][-xlen]/sum(x[[i]][-xlen]),1)
    cat("%!NA",formatC(as.character(percentages),width=maxchar),"\n")
   }
  }
  cat("\n")
 }
}
