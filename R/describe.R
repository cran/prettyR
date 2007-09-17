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
 if(vname.space) cat(formatC(varname,width=-vname.space))
 for(i in 1:desclen)
  desc.vector[i]<-do.call(num.desc[i],list(x,na.rm=TRUE))
 if(fname.space) cat(formatC(desc.vector,width=fname.space),"\n")
 return(desc.vector)
}

describe.factor<-function(x,varname="",vname.space=10,maxfac=10,show.pc=TRUE) {
 lenx<-length(x)
 factab<-table(x)
 tablen<-length(factab)
 vnx<-valid.n(x)
 maxtab<-ifelse(tablen>maxfac,maxfac,tablen)
 if(lenx > vnx) {
  NAtab<-lenx-vnx
  names(NAtab)<-"NA"
  factab<-c(factab,NAtab)
  maxtab<-maxtab+1
 }
 fname.space<-max(nchar(names(factab)[1:maxtab]))+1
 if(fname.space<8) fname.space<-8
 cat(paste(rep(" ",vname.space),sep="",collapse=""),
  formatC(names(factab)[1:maxtab],width=fname.space),"\n",sep="")
 cat(formatC(varname,width=-vname.space),sep="")
 modex<-Mode(x)
 cat(formatC(factab[1:maxtab],width=fname.space),"\n")
 if(show.pc) {
  cat(formatC("Percent",width=-vname.space),sep="")
  cat(formatC(round(100*factab[1:maxtab]/length(x),2),width=vname.space),"\n")
 }
 cat("mode = ",modex,"  Valid n = ",vnx,sep="")
 if(maxtab<tablen) cat("  ",tablen,"categories - only first",maxfac,"shown")
 cat("\n\n")
 return(c(modex,vnx))
}

describe.logical<-function(x,varname="",vname.space=10) {
 cat(formatC(varname,width=-vname.space),sep="")
 logjam<-c(as.numeric(table(x)),sum(is.na(x)))
 cat(formatC(logjam,width=10),"\n")
 return(logjam)
}

describe<-function(x,num.desc=c("mean","median","var","sd","valid.n"),
 xname=NA,maxfac=8) {

 if(missing(x)) stop("Usage: describe(x,...)\n\twhere x is a vector, data frame or matrix")
 if(!is.data.frame(x)) x<-as.data.frame(x)
 varnames<-names(x)
 if(is.null(varnames)) varnames<-paste("V",1:dim(x)[2],sep="")
 if(is.data.frame(x)) {
  if(is.na(xname)) xname<-deparse(substitute(x))
  cat("Description of",xname,"\n")
  num.index<-which(sapply(x,is.numeric))
  nnum<-length(num.index)
  if(nnum) {
   num.result<-matrix(NA,nrow=nnum,ncol=length(num.desc))
   rownames(num.result)<-varnames[num.index]
   colnames(num.result)<-num.desc
   vname.space<-max(nchar(varnames[num.index]))+1
   if(vname.space<8) vname.space<-8
   fname.space<-max(nchar(num.desc))+1
   # this allows for large numbers that will use scientific notation
   if(fname.space<10) fname.space<-10
   cat("\nNumeric\n",paste(rep(" ",vname.space),sep="",collapse=""),
    formatC(num.desc,width=fname.space),"\n",sep="")
   for(col in 1:nnum)
    num.result[col,]<-describe.numeric(x[[num.index[col]]],num.desc=num.desc,
     varname=varnames[num.index[col]],vname.space=vname.space,
     fname.space=fname.space)
  }
  else num.result<-NULL
  fac.index<-which(sapply(x,is.factor))
  nfac<-length(fac.index)
  if(nfac) {
   fac.result<-matrix(NA,nrow=nfac,ncol=2)
   rownames(fac.result)<-varnames[fac.index]
   colnames(fac.result)<-c("Mode","N")
   vname.space<-max(nchar(varnames[fac.index]))+1
   if(vname.space<8) vname.space<-8
   cat("\nFactor\n")
   for(col in 1:nfac)
    fac.result[col,]<-describe.factor(x[[fac.index[col]]],
     varname=varnames[fac.index[col]],
     vname.space=vname.space,maxfac=maxfac)
  }
  else fac.result<-NULL
  log.index<-which(sapply(x,is.logical))
  nlog<-length(log.index)
  if(nlog) {
   vname.space<-max(nchar(varnames[log.index]))+1
   if(vname.space<8) vname.space<-8
   cat("\nLogical\n",paste(rep(" ",vname.space),sep="",collapse=""))
   cat("    FALSE       TRUE         NA\n")
   log.result<-matrix(NA,nrow=nlog,ncol=3)
   rownames(log.result)<-varnames[log.index]
   colnames(log.result)<-c("False","True","Missing")
   for(col in 1:nlog)
    log.result[col,]<-describe.logical(x[[log.index[col]]],
     varname=varnames[log.index[col]],vname.space=vname.space)
  }
  else log.result<-NULL
  return(list(Numeric=num.result,Factor=fac.result,Logical=log.result))
 }
 else cat("describe: x must be a vector, matrix or data frame\n")
}
