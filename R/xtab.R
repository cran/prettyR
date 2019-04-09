xtab<-function(formula,data,varnames=NULL,or=TRUE,chisq=FALSE,phi=FALSE,
 html=FALSE,bgcol="lightgray",lastone=TRUE) {

 if(missing(formula))
  stop("Usage: xtab(formula,data,varnames=NULL,or=TRUE,chisq=FALSE,phi=FALSE\n")
 ft<-as.character(attr(terms(formula),"variables")[-1])
 nft<-length(ft)
 xtnames<-ft[1]
 if(nft>2) {
  xt<-list()
  # use the last variable name in the formula first
  by.factor<-as.factor(data[[ft[nft]]])
  factor.levels<-levels(by.factor)
  factor.labels<-attr(data[,ft[nft]],"value.labels")
  if(is.null(factor.labels)) factor.labels<-factor.levels
  nlevels<-length(factor.levels)
  for(i in 1:nlevels) {
   currentdata<-subset(data,by.factor==factor.levels[i])
   for(j in 1:dim(currentdata)[2])
    attr(currentdata[,j],"value.labels")<-attr(data[,j],"value.labels")
   currentcount<-length(currentdata[[nft]])
   totalcount<-length(data[[nft]])
   # delete the last variable name
   rightside<-ifelse(nft>3,paste(ft[2:(nft-1)],sep="",collapse="+"),ft[2])
   next.formula<-as.formula(paste(ft[1],rightside,sep="~",collapse=""))
   xt[[i]]<-xtab(next.formula,data=currentdata,varnames=varnames,chisq=chisq,
    phi=phi,html=html,bgcol=bgcol,lastone=FALSE)
   xtnames<-c(xtnames,paste0(ft[nft],"_",names(factor.labels)[i]))
  }
 }
 else {
  if(is.null(varnames)) varnames<-ft
  if(missing(data)) xt<-calculate.xtab(get(ft[1]),get(ft[2]),varnames=varnames)
  else xt<-calculate.xtab(data[,ft[1]],data[,ft[2]],varnames=varnames)
 }
 if(lastone) {
  if(length(xtnames) > 1) names(xt)<-xtnames[-1]
  else names(xt)<-c("counts","row.margin","col.margin","varnames")
 }
 return(xt)
}
