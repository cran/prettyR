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

brkdn<-function(formula,dataframe,maxlevels=10,do.sd=FALSE) {
 if(!missing(dataframe) && !missing(formula)) {
  bn<-as.character(attr(terms(formula),"variables")[-1])
  nbn<-length(bn)
  cat("\nBreakdown of",bn[1],"by",bn[nbn],"\n")
  if(!is.numeric(dataframe[[bn[1]]]))
   stop("\nVariable on left of formula must be numeric")
  if(nbn > 2) {
   # get the factor for this level
   by.factor<-as.factor(dataframe[[bn[nbn]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   brkstats<-as.list(rep(0,nlevels))
   names(brkstats)<-factor.levels
   # calculate the mean for this level
   for(i in 1:nlevels) {
    currentdata<-subset(dataframe,by.factor == factor.levels[i])
    cat(paste("\nVariable",bn[1],sep=" ",collapse=""),"for",bn[nbn],"- level",factor.levels[i],"\n\n")
    cat("Mean     ",mean(currentdata[bn[1]],na.rm=TRUE),"\n")
    if(do.sd) cat("Std. dev.",sd(currentdata[bn[1]],na.rm=TRUE),"\n")
    else cat("Variance ",var(currentdata[bn[1]],na.rm=TRUE),"\n")
    cat("n        ",sum(!is.na(currentdata[bn[1]])),"\n")
    next.formula<-as.formula(paste(paste(bn[1],"~"),paste(bn[2:(nbn-1)],collapse="+")))
    # and call yourself for the next level down
    brkstats[[i]]<-brkdn(next.formula,currentdata,do.sd=do.sd)
   }
   class(brkstats)<-"dstat"
   invisible(brkstats)
  }
  else {
   by.factor<-as.factor(dataframe[[bn[2]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   gstats<-matrix(NA,ncol=nlevels,nrow=3)
   colnames(gstats)<-factor.levels[1:nlevels]
   if(do.sd) rownames(gstats)<-c("Mean","SD","n")
   else rownames(gstats)<-c("Mean","Variance","n")
   # calculate the basic descriptive stats
   if(is.numeric(dataframe[[bn[1]]])) {
    for(i in 1:nlevels) {
     currentdata<-subset(dataframe[[bn[1]]],by.factor == factor.levels[i])
     if(length(currentdata)) gstats[,i]<-dstats(currentdata,do.sd=do.sd)
    }
    class(gstats)<-"dstat"
    print(gstats)
   }
   invisible(gstats)
  }
 }
 else cat("Usage: brkdn(formula,dataframe,maxlevels=10,do.sd=FALSE)\n")
}
