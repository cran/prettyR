Mode<-function(x,na.rm) {
 xtab<-table(x)
 xmode<-names(which(xtab == max(xtab)))
 if(length(xmode) > 1) xmode<-">1 mode"
 return(xmode)
}

valid.n<-function(x,na.rm) {
 return(sum(!is.na(x)))
}

describe.numeric<-function(x,num.desc=c("mean","median","var","sd","valid.n"),
 varname="",vname.space=10,fname.space=10) {

 desclen<-length(num.desc)
 desc.vector<-rep(0,desclen)
 cat(formatC(varname,width=-vname.space))
 for(i in 1:desclen) {
  desc.vector<-do.call(num.desc[i],list(x,na.rm=TRUE))
  cat(formatC(desc.vector,width=fname.space))
 }
 cat("\n")
 return(desc.vector)
}

describe.factor<-function(x,varname="",vname.space=10) {
 factab<-table(x)
 tablen<-length(factab)
 maxtab<-ifelse(tablen>8,8,tablen)
 fname.space<-max(nchar(names(factab)[1:maxtab]))+1
 if(fname.space<8) fname.space<-8
 cat(paste(rep(" ",vname.space),sep="",collapse=""),
  formatC(names(factab)[1:maxtab],width=fname.space),"\n",sep="")
 cat(formatC(varname,width=-vname.space),sep="")
 cat(formatC(factab[1:maxtab],width=fname.space),"\nmode = ",Mode(x),"  Valid n = ",
  valid.n(x),sep="")
 if(maxtab<tablen) cat("  ",tablen,"categories - only first 8 shown")
 cat("\n\n")
}

describe.logical<-function(x,varname="",vname.space=10) {
 cat(formatC(varname,width=-vname.space),sep="")
 cat(formatC(c(as.numeric(table(x)),sum(is.na(x))),width=10),"\n")
}

describe<-function(x,num.desc=c("mean","median","var","sd","valid.n"),show.num.fac=FALSE,
 xname=NA) {

 if(missing(x)) stop("Usage: describe(x,...)\n\twhere x is a vector, data frame or matrix")
 if(!is.data.frame(x)) x<-as.data.frame(x)
 varnames<-names(x)
 if(is.null(varnames)) varnames<-paste("V",1:dim(x)[2],sep="")
 if(is.data.frame(x)) {
  if(is.na(xname)) xname<-deparse(substitute(x))
  cat("Description of",xname,"\n")
  num.index<-which(sapply(x,is.numeric))
  if(length(num.index)) {
   vname.space<-max(nchar(varnames[num.index]))+1
   if(vname.space<8) vname.space<-8
   fname.space<-max(nchar(num.desc))+1
   # this allows for large numbers that will use scientific notation
   if(fname.space<10) fname.space<-10
   cat("\nNumeric\n",paste(rep(" ",vname.space),sep="",collapse=""),
    formatC(num.desc,width=fname.space),"\n",sep="")
   for(col in 1:length(num.index))
    describe.numeric(x[[num.index[col]]],num.desc=num.desc,
     varname=varnames[num.index[col]],vname.space=vname.space,fname.space=fname.space)
  }
  if(show.num.fac) {
   options(warn=-1)
   numfac.index<-which(sapply(x,is.numeric.factor))
   options(warn=0)
   if(length(numfac.index)) {
    vname.space<-max(nchar(varnames[numfac.index]))+1
    if(vname.space<8) vname.space<-8
    fname.space<-max(nchar(num.desc))+1
    if(fname.space<8) fname.space<-8
    cat("\nNumeric factor\n",paste(rep(" ",vname.space),sep="",collapse=""),
     formatC(num.desc,width=fname.space),"\n",sep="")
    for(col in 1:length(numfac.index))
     describe.numeric(as.numeric(x[[numfac.index[col]]]),num.desc=num.desc,
      varname=varnames[numfac.index[col]],vname.space=vname.space,fname.space=fname.space)
   }
  }
  fac.index<-which(sapply(x,is.factor))
  if(length(fac.index)) {
   vname.space<-max(nchar(varnames[fac.index]))+1
   if(vname.space<8) vname.space<-8
   cat("\nFactor\n")
   for(col in 1:length(fac.index))
    describe.factor(x[[fac.index[col]]],varname=varnames[fac.index[col]],
     vname.space=vname.space)
  }
  log.index<-which(sapply(x,is.logical))
  if(length(log.index)) {
   vname.space<-max(nchar(varnames[log.index]))+1
   if(vname.space<8) vname.space<-8
   cat("\nLogical\n",paste(rep(" ",vname.space),sep="",collapse=""))
   cat("    FALSE       TRUE         NA\n")
   for(col in 1:length(log.index))
    describe.logical(x[[log.index[col]]],varname=varnames[log.index[col]],
     vname.space=vname.space)
  }
 }
 else cat("describe: x must be a vector, matrix or data frame\n")
}
