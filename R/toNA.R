toNA<-function(x,values=NA) {
 if(missing(x))
  stop("Usage: toNA(x,values=NA)\n\twhere value is one or more values to be set to NA")
 if(is.vector(x) || is.matrix(x)) return(x[x%in%values]<-NA)
 if(is.data.frame(x)) {
  xdim<-dim(x)
  if(length(xdim) < 3) {
   for(col in 1:xdim[2]) {
    rows<-x[,col]%in%values  
    x[rows,col]<-x[rows,col]<-NA
   }
  }
  else warning("toNA: maximum 2D data frames")
  return(x)
 }
 warning("toNA: x must be a vector, matrix or data frame")
 return(x)
}
