# brkdn is a function that attempts to calculate and display means, 
# variances (or standard deviations if do.sd is TRUE) and valid ns 
# for the variable that appears on the left side of the formula.
# It expects the variables on the right side of the formula to be
# factors, or at worst integers with a small number of unique values.
# It returns a list of "dstat" objects (or a list of lists of "dstat"
# objects if there are two breakdown variables, and so on.)
# A "dstat" object is a matrix that looks like:
#                       vname1  vname2  ...
#       Mean            mean1   mean2   ...
#       Variance        var1    var2    ...
#       Valid n         n1      n2      ...

brkdn<-function(formula,data,maxlevels=10,
 num.desc=c("mean","var","valid.n")) {
 
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
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   brkstats<-vector("list",nlevels)
   names(brkstats)<-factor.levels
   # calculate the mean for this level
   for(i in 1:nlevels) {
    currentdata<-subset(data,by.factor == factor.levels[i])
    cat(paste("\nVariable",bn[1],sep=" ",collapse=""),"for",bn[nbn],
     "- level",factor.levels[i],"\n\n")
    cat(formatC(num.desc,width=10),"\n")
    junk<-describe.numeric(currentdata[bn[1]],num.desc=num.desc,vname.space=0)
    next.formula<-
     as.formula(paste(paste(bn[1],"~"),paste(bn[2:(nbn-1)],collapse="+")))
    # and call yourself for the next level down
    brkstats[[i]]<-brkdn(next.formula,currentdata,num.desc=num.desc)
    class(brkstats[[i]])<-"dstat"
   }
   class(brkstats)<-"dstat"
   invisible(brkstats)
  }
  else {
   by.factor<-as.factor(data[[bn[2]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   gstats<-matrix(NA,ncol=nlevels,nrow=3)
   colnames(gstats)<-factor.levels[1:nlevels]
   rownames(gstats)<-num.desc
   # calculate the basic descriptive stats
   if(is.numeric(data[[bn[1]]])) {
    for(i in 1:nlevels) {
     currentdata<-subset(data[[bn[1]]],by.factor == factor.levels[i])
     if(length(currentdata))
      gstats[,i]<-describe.numeric(currentdata,num.desc=num.desc,
       vname.space=0,fname.space=0)
    }
    class(gstats)<-"dstat"
    cat("         ",formatC(colnames(gstats),width=10),"\n")
    rnames<-rownames(gstats)
    for(i in 1:length(rnames)) {
     if(rnames[i] == "valid.n")
      cat(formatC(rnames[i],width=9),formatC(round(gstats[i,],0),width=10),"\n")
     else cat(formatC(rnames[i],width=9),formatC(gstats[i,],width=10),"\n")
    }
   }
   invisible(gstats)
  }
 }
 else
  cat("Usage: brkdn(formula,data,maxlevels=10,num.desc=c(\"mean\",\"var\",\"valid.n\"))\n")
}
