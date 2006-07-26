# dstats calculates the means, variances and valid ns for the
# numeric columns of the matrix or data frame x
# It returns an object of class "dstat", a matrix that looks like:
#			name1	name2	...
#	Mean		mean1	mean2	...
#	Variance	var1	var2	...
#	Valid n		n1	n2	...
# If a vector is passed as x, a one column matrix will be returned.
# if show.sd is TRUE, standard deviation will be returned instead of variance

dstats<-function(x,indices=NA,do.sd=FALSE) {
 if(!missing(x)) {
  if(is.data.frame(x) | is.matrix(x)) {
# include only numeric components, exclude factors
   if(is.na(indices))
    indices<-which(sapply(x,is.numeric))
   d1<-sapply(x[,indices],mean,na.rm=TRUE)
   d2<-sapply(x[,indices],var,na.rm=TRUE)
   if(do.sd) d2<-sqrt(d2)
   d3<-sapply(x[,indices],function(x){return(sum(!is.na(x)))})
   dstat<-(rbind(d1,d2,d3))
  }
  else {
   dstat<-matrix(0,nrow=3,ncol=1)
   dstat[1,1]<-mean(x,na.rm=TRUE)
   dstat[2,1]<-var(x,na.rm=TRUE)
   if(do.sd) dstat[2,1]<-sqrt(dstat[2,1])
   dstat[3,1]<-sum(!is.na(x))
  }
  if(do.sd) rownames(dstat)<-c("Mean","SD","n")
  else rownames(dstat)<-c("Mean","Variance","n")
  class(dstat)<-"dstat"
  return(dstat)
 }
 cat("Usage: dstats(x,indices=NA,do.sd=FALSE)\n")
 cat("\twhere x is a data frame, matrix or vector\n")
}
