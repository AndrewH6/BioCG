#' Not needed likely as the calcs aren't so ownerous that we cant just use catkabelmat

#' @export

catkabelmat2=function(ytable,mylab,decimals=1){
  #Not needed likely as the calcs aren't so ownerous that we cant just use catkabelmat
  #Haven't checked the data threshold
  #Provides a nice kabel table for a table named ytable that already has counts % (ie as output from cattable)
  #mylab is a label to print out to keep with the results, ie could be a nice name for y
  #decimals is the number of decimals to print out
  #returns a matrix of counts and %
  mymat=as.data.frame(cbind(rep(mylab,nrow(ytable)),ytable[,1],ytable[,2],#formatC(ytable[,2],big.mark=","),
                            round(ytable[,3],digits=decimals)))
  names(mymat)=c("C0","C1","C2","C3")
  #return(mymat)
}
