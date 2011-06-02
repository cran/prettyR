skew<-function(x) {
 # get rid of NAs, worry about the effects later
 x<-x[!is.na(x)]
 # get the deviations
 devx<-x-mean(x)
 # get the sample standard deviation
 sdx<-sd(x)
 # calculate and return the result
 return(sum(devx^3)/((length(x)-1)*sdx^3))
}
