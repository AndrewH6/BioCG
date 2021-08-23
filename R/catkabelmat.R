#' Provides a nice kabel table to summarize a categorical variable supplied in y

#' @export

catkabelmat=function(y,mylab,decimals=1,check5=F,print5=T){
  #Provides a nice kabel table to summarize a categorical variable supplied in y
  #mylab is a label to print out to keep with the results, ie could be a nice name for y
  #decimals is the number of decimals to print out
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts
  #returns a matrix of counts and %
  BCthresh=5 #threshold for checking what is too small to leave CaraSpace

  check5=F
  print5=T

  less5=0 #flag if there is a non-missing value that is <BCthresh
  if(min(table(y))<BCthresh) {
    less5=1
    cat("\n\n********* WARNING: A count is less than",BCthresh,"and values will be suppressed.\n\n")  }

  xx=table(y,useNA="ifany")
  names(xx)[is.na(names(xx))==T]="NA"
  totalx=sum(xx)
  percentxx=format(round(100*xx/totalx,decimals),nsmall=decimals)
  percentxxformat=percentxx;#  paste("(",percentxx,")",sep="")
  mymat=as.data.frame(cbind(rep(mylab,length(xx)),names(xx),formatC(xx,big.mark=","),percentxxformat))
  mymat$V1=as.character(mymat$V1);mymat$V2=as.character(mymat$V2);mymat$V3=as.character(mymat$V3)
  mymat$percentxxformat=as.character(mymat$percentxxformat)
  for(i in 1:nrow(mymat)){
    if(mymat[i,2]!="NA" && print5==F && less5==1){
      if(xx[i]<BCthresh){
        mymat[i,3]="suppressed"
        mymat[i,4]="suppressed"
      }
    }
  }
  names(mymat)=c("C0","C1","C2","C3")
  return(mymat)
}
