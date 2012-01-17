truncString<-function(x,maxlen=20,justify="left") {
 ncharx<-nchar(x)
 toolong<-ncharx > maxlen
 maxwidth<-ifelse(toolong,maxlen-3,maxlen)
 chopx<-substr(x,1,maxwidth)
 lenx<-length(x)
 for(i in 1:length(x)) if(toolong[i]) chopx[i]<-paste(chopx[i],"...",sep="")
 return(formatC(chopx,width=maxlen,flag=ifelse(justify=="left","-"," ")))
}
