# brkdn is a function that attempts to calculate the summary measures 
# that are included in num.desc
# for the variable that appears on the left side of the formula.
# It expects the variables on the right side of the formula to be
# factors, or at worst integers with a small number of unique values.
# It returns a list of "dstat" objects (or a list of lists of "dstat"
# objects if there are two breakdown variables, and so on.)
# A "dstat" object is a matrix that looks like:
#                       vname1  vname2  ...
#       mean            mean1   mean2   ...
#       variance        var1    var2    ...
#       Valid n         n1      n2      ...

brkdn<-function(formula,data,maxlevels=10,num.desc=c("mean","var","valid.n"),
 vname.width=NULL,width=10,round.n=2) {
 
 if(!missing(data) && !missing(formula)) {
  bn<-as.character(attr(terms(formula),"variables")[-1])
  nbn<-length(bn)
  cat("\nBreakdown of",bn[1],"by",bn[nbn],"\n")
  if(!is.numeric(data[[bn[1]]]))
   stop("\nVariable on left of formula must be numeric")
  if(nbn > 2) {
   # get the factor for this level
   by.factor<-as.factor(data[[bn[nbn]]])
   factor.levels<-levels(by.factor)
   factor.labels<-attr(data[[bn[nbn]]],"value.labels")
   if(!is.null(names(factor.labels))) factor.labels<-names(factor.labels)
   if(is.null(factor.labels)) factor.labels<-factor.levels
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   brkstats<-vector("list",nlevels)
   names(brkstats)<-factor.levels[1:nlevels]
   # calculate the mean for this level
   for(i in 1:nlevels) {
    currentdata<-subset(data,by.factor == factor.levels[i])
    for(j in 1:dim(currentdata)[2])
     attr(currentdata[,j],"value.labels")<-attr(data[,j],"value.labels")
    cat(paste("\nVariable",bn[1],sep=" ",collapse=""),"for",bn[nbn],
     "- level",factor.labels[i],"\n\n")
    cat(formatC(num.desc,width=10),"\n")
    junk<-describe.numeric(currentdata[bn[1]],num.desc=num.desc,
     fname.space=width,vname.space=0)
    next.formula<-
     as.formula(paste(paste(bn[1],"~"),paste(bn[2:(nbn-1)],collapse="+")))
    # and call yourself for the next level down
    brkstats[[i]]<-brkdn(next.formula,currentdata,maxlevels=maxlevels,
     num.desc=num.desc)
    class(brkstats[[i]])<-"dstat"
   }
   class(brkstats)<-"dstat"
   invisible(brkstats)
  }
  else {
   by.factor<-as.factor(data[[bn[2]]])
   factor.levels<-levels(by.factor)
   factor.labels<-attr(data[[bn[2]]],"value.labels")
   if(!is.null(names(factor.labels))) factor.labels<-names(factor.labels)
   if(is.null(factor.labels)) factor.labels<-factor.levels
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   gstats<-matrix(NA,ncol=nlevels,nrow=length(num.desc))
   colnames(gstats)<-factor.labels[1:nlevels]
   rownames(gstats)<-num.desc
   # calculate the basic descriptive stats
   if(is.numeric(data[[bn[1]]])) {
    round.ns<-rep(round.n,length(num.desc))
    npos<-match("valid.n",num.desc)
    if(!is.na(npos)) round.ns[npos]<-0
    cat(formatC("Level",width=vname.width))
    cat(formatC(num.desc,width=width),"\n")
    if(is.null(vname.width)) vname.width<-max(nchar(factor.labels))
    for(i in 1:nlevels) {
     currentdata<-subset(data[[bn[1]]],by.factor == factor.levels[i])
     if(length(currentdata)) {
      gstats[,i]<-describe.numeric(currentdata,num.desc=num.desc,
       vname.space=0,fname.space=0)
      cat(formatC(factor.labels[i],width=vname.width),
       formatC(round(gstats[,i],round.ns),width=width),"\n")
     }
    }
    class(gstats)<-"dstat"
    rnames<-rownames(gstats)
   }
   invisible(gstats)
  }
 }
 else
  cat("Usage: brkdn(formula,data,maxlevels=10,num.desc=c(\"mean\",\"var\",\"valid.n\"))\n")
}
