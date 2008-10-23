print.freq<-function(x,show.pc=TRUE,...) {
 nfreq<-length(x)
 variable.labels <- names(x)
 for(i in 1:nfreq) {
  categories<-names(x[[i]])
  maxchar<-max(c(nchar(categories),4))
  cat("\nFrequencies for", variable.labels[i],"\n")
  cat("    ", formatC(ifelse(categories == "","Missing",categories),width=maxchar),"\n")
  cat("    ", formatC(as.character(x[[i]]),width = maxchar),"\n")
  if(show.pc) {
   percentages<-round(100*x[[i]]/sum(x[[i]]),1)
   cat("%   ",formatC(as.character(percentages),width=maxchar),"\n")
   if(any(names(x[[i]]) == "NA")) {
    xlen<-length(x[[i]])
    nna<-x[[i]][xlen]
    percentages<-round(100*x[[i]][-xlen]/sum(x[[i]][-xlen]),1)
    cat("%!NA",formatC(as.character(percentages),width = maxchar),"\n")
   }
  }
  cat("\n")
 }
}
