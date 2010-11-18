truncString<-function(x,maxlen=20) {
 ncharx<-nchar(x)
 toolong<-ncharx>maxlen
 maxwidth<-ifelse(toolong,maxlen-3,maxlen)
 chopx<-substr(x,1,maxwidth)
 lenx<-length(x)
 tooshort<-rep("",lenx)
 for(i in 1:lenx) {
  if(ncharx[i] < maxlen)
   tooshort[i]<-paste(rep(" ",maxlen-ncharx[i]),collapse="")
 }
 suffix<-ifelse(toolong,"...",tooshort)
 return(paste(chopx,suffix,sep=""))
}
