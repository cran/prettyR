print.xtab<-function(x,col.width=8,chisq=FALSE,phi=FALSE,...) {
 cat("\nCrosstabulation of",x$varnames[1],"by",x$varnames[2],"\n")
 rowname.width<-max(nchar(c(x$varnames[1],names(x$row.margin))))
 row.labels<-formatC(names(x$row.margin),width=-rowname.width)
 rowname.space<-paste(rep(" ",rowname.width),sep="",collapse="")
 cat(rowname.space,x$varnames[2],"\n")
 maxcolab<-max(nchar(names(x$col.margin)))
 if(maxcolab>col.width) col.width<-maxcolab+1
 col.labels<-formatC(names(x$col.margin),width=col.width)
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
