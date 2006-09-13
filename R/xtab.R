calculate.xtab<-function(v1,v2,varnames=NULL) {
 counts<-table(v1,v2)
 if(is.null(varnames)) {
  rowname<-deparse(substitute(v1))
  colname<-deparse(substitute(v2))
 }
 else {
  rowname<-varnames[1]
  colname<-varnames[2]
 }
 row.margin<-apply(counts,1,sum)
 col.margin<-apply(counts,2,sum)
 xt<-list(counts=counts,row.margin=row.margin,col.margin=col.margin,
  varnames=c(rowname,colname))
 return(xt)
}

# xtab will try to break down the formula passed to it into
# one or more 2D crosstabulations with hierarchical counts
# for higher level factors.

xtab<-function(formula,data,varnames=NULL,chisq=FALSE,phi=FALSE) {
 if(missing(formula))
  stop("Usage: xtab(formula,data,varnames=NULL,chisq=FALSE,phi=FALSE)\n")
 ft<-as.character(attr(terms(formula),"variables")[-1])
 nft<-length(ft)
 if(nft > 2) {
  by.factor<-as.factor(data[[ft[nft]]])
  factor.levels<-levels(by.factor)
  nlevels<-length(factor.levels)
  brkstats<-as.list(rep(0,nlevels))
  names(brkstats)<-factor.levels
  for(i in 1:nlevels) {
   currentdata<-subset(data,by.factor == factor.levels[i])
   currentcount<-length(currentdata[[nft]])
   totalcount<-length(data[[nft]])
   cat("\nCount for",ft[nft],"=",factor.levels[i],"is",
    currentcount,"(",round(100*currentcount/totalcount,1),"%)\n\n")
   rightside <-ifelse(nft > 3,paste(ft[2:(nft-1)],sep="",collapse="+"),ft[2])
   next.formula<-
    as.formula(paste(ft[1],rightside,sep="~",collapse=""))
   xtab(next.formula,data=currentdata,varnames=varnames,chisq=chisq,phi=phi)
  }
 }
 else {
  if(missing(data)) xt<-calculate.xtab(get(ft[1]),get(ft[2]),varnames=ft)
  else xt<-calculate.xtab(data[[ft[1]]],data[[ft[2]]],varnames=ft)
  print.xtab(xt,chisq=chisq,phi=phi)
  invisible(xt)
 }
}

print.xtab<-function(x,col.width=8,chisq=FALSE,phi=FALSE,...) {
 cat("\nCrosstabulation of",x$varnames[1],"by",x$varnames[2],"\n")
 rowname.width<-max(nchar(c(x$varnames[1],dimnames(x$counts)[[1]])))
 row.labels<-formatC(dimnames(x$counts)[[1]],width=-rowname.width)
 rowname.space<-paste(rep(" ",rowname.width),sep="",collapse="")
 cat(rowname.space,x$varnames[2],"\n")
 maxcolab<-max(nchar(dimnames(x$counts)[[2]]))
 if(maxcolab>col.width) col.width<-maxcolab+1
 col.labels<-formatC(dimnames(x$counts)[[2]],width=col.width)
 cat(formatC(x$varnames[1],width=-rowname.width),col.labels,"\n")
 tdim<-dim(x$counts)
 gt<-sum(x$counts)
 for(i in 1:tdim[1]) {
  cat(row.labels[i],formatC(c(x$counts[i,],x$row.margin[i]),width=col.width),"\n")
  cat(rowname.space,formatC(100*c(x$counts[i,]/x$row.margin[i],x$row.margin[i]/gt),width=col.width),"\n")
  cat(rowname.space,formatC(100*x$counts[i,]/x$col.margin,width=col.width),"\n\n")
 }
 cat(rowname.space,formatC(c(x$col.margin,gt),width=col.width),"\n")
 cat(rowname.space,formatC(100*x$col.margin/gt,width=col.width),"\n\n")
 if(chisq) {
  x2<-chisq.test(x$counts,...)
  cat("X2[",x2$parameter,"] = ",x2$statistic,", p = ",x2$p.value,"\n\n",sep="")
 }
 if(phi) {
  if(tdim[1] == 2 && tdim[2] == 2) {
   num<-x$counts[1,1]*x$counts[2,2] - x$counts[1,2]*x$counts[2,1]
   denom<-sqrt(x$row.margin[1]*x$row.margin[2]*x$col.margin[1]*x$col.margin[2])
   cat("phi =",num/denom,"\n")
  }
  else cat("phi coefficient only valid for 2x2 table\n")
 }
}
