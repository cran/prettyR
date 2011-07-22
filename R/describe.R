Mode<-function(x,na.rm) {
 xtab<-table(x)
 xmode<-names(which(xtab == max(xtab)))
 if(length(xmode) > 1) xmode<-">1 mode"
 return(xmode)
}

valid.n<-function(x,na.rm=TRUE) {
 return(ifelse(na.rm,sum(!is.na(x)),length(x)))
}

describe.numeric<-function(x,num.desc=c("mean","median","var","sd","valid.n"),
 varname="",vname.space=20,fname.space=20) {

 desclen<-length(num.desc)
 desc.vector<-rep(0,desclen)
 if(vname.space) cat(truncString(varname,maxlen=vname.space))
 for(i in 1:desclen) {
  if(valid.n(x))
   desc.vector[i]<-do.call(num.desc[i],list(x,na.rm=TRUE))
  else desc.vector[i]<-NA
 }
 if(fname.space) cat(formatC(desc.vector,width=fname.space),"\n",sep="")
 return(desc.vector)
}

describe.factor<-function (x,varname="",vname.space=20,
 fname.space=30,maxfac=10,show.pc=TRUE,horizontal=FALSE) {

 lenx <- length(x)
 factab <- table(x)
 tablen <- length(factab)
 vnx <- valid.n(x)
 maxtab <- ifelse(tablen > maxfac, maxfac, tablen)
 if(lenx > vnx) {
  NAtab <- lenx - vnx
  names(NAtab) <- "NA"
  factab <- c(factab, NAtab)
  maxtab <- maxtab + 1
 }
 modex <- Mode(x)
 if(horizontal) {
  cat(paste(rep(" ", vname.space), sep = "", collapse = ""), 
   truncString(names(factab)[1:maxtab],fname.space), 
   "\n", sep = "")
  cat(formatC(varname, width = -vname.space), sep = "")
  cat(formatC(factab[1:maxtab],width=fname.space),"\n",sep="")
  if (show.pc) {
   cat(formatC("Percent", width = -vname.space), sep = "")
   cat(formatC(round(100 * factab[1:maxtab]/length(x), 2), 
    width = fname.space), "\n", sep = "")
  }
 }
 else {
  facorder<-order(factab,decreasing=TRUE)
  faclabels<-truncString(names(factab),fname.space)[facorder]
  cat("\n",varname,"\nValue",rep(" ",nchar(faclabels[1])),"   Count Percent\n",sep="")
  faccounts<-formatC(factab,width=8)[facorder]
  facpct<-formatC(round(100*factab/length(x),2),width=8)[facorder]
  for(facval in 1:maxtab) {
   cat(faclabels[facval],faccounts[facval],facpct[facval],"\n")
  }
 }
 cat("mode = ",modex,"  Valid n = ",vnx,sep="")
 if(maxtab < tablen) 
  cat("  ",tablen,"categories - only first",maxfac,"shown")
 cat("\n")
 return(c(modex,vnx))
}

describe.logical<-function(x,varname="",vname.space=20,show.pc=TRUE) {

 cat(formatC(varname,width=-vname.space),sep="")
 nmiss<-sum(is.na(x))
 if(all(is.na(x))) {
  logjam<-c(0,0)
  pctrue<-0
 }
 else {
  logjam<-c(as.numeric(table(x)))
  pctrue<-100*logjam[2]/sum(logjam)
 }
 cat(formatC(logjam,width=10))
 if(show.pc) cat(formatC(pctrue,width=10))
 cat(formatC(nmiss,width=10),"\n")
 return(c(logjam,nmiss))
}

describe<-function(x,num.desc=c("mean","median","var","sd","valid.n"),
 xname=NA,vname.space=20,fname.space=30,maxfac=10,show.pc=TRUE,
 horizontal=FALSE) {

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
   nopdigits<-options("digits")$digits
   options(digits=4)
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
   options(digits=nopdigits)
  }
  else num.result<-NULL
  fac.index<-c(which(sapply(x,is.factor)),which(sapply(x,is.character)))
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
     varname=varnames[fac.index[col]],vname.space=vname.space,
     fname.space=fname.space,maxfac=maxfac,show.pc=show.pc,
     horizontal=horizontal)
  }
  else fac.result<-NULL
  log.index<-which(sapply(x,is.logical))
  nlog<-length(log.index)
  if(nlog) {
   vname.space<-max(nchar(varnames[log.index]))+1
   if(vname.space<8) vname.space<-8
   cat("\nLogical\n",paste(rep(" ",vname.space),sep="",collapse=""))
   cat("    FALSE       TRUE")
   if(show.pc) cat("     %TRUE")
   cat("        NA\n")
   log.result<-matrix(NA,nrow=nlog,ncol=3)
   rownames(log.result)<-varnames[log.index]
   colnames(log.result)<-c("False","True","Missing")
   for(col in 1:nlog)
    log.result[col,]<-describe.logical(x[[log.index[col]]],
     varname=varnames[log.index[col]],vname.space=vname.space,
     show.pc=show.pc)
  }
  else log.result<-NULL
  invisible(list(Numeric=num.result,Factor=fac.result,Logical=log.result))
 }
 else cat("describe: x must be a vector, matrix or data frame\n")
}
