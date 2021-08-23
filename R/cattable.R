#' returns a data.frame of counts and percents corresponding to the data y that is good for exporting or printing with knitr::kable

#' @export

cattable=function(y,mydigits=1,check5=F,print5=T){
  #returns a data.frame of counts and percents corresponding to the data y that is good for exporting or printing with knitr::kable
  #assumes that missing data are NA, all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #checks to see if any counts are too small to exit CaraSpace; "suppressed" is the label used if that is the case
  #y=vector containing the data
  #mydigits=number of decimal places to display in the table
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts

  BCthresh=5 #threshold for checking what is too small to leave CaraSpace

  check5=F
  print5=T

  less5=0 #flag if there is a non-missing value that is <BCthresh
  if(min(table(y))<BCthresh) {
    less5=1
    cat("\n\n********* WARNING: A count is less than",BCthresh,"and values will be suppressed.\n\n")  }

  xx=table(y, useNA = "ifany")
  xxpercent=round(100*xx/sum(xx),mydigits)
  nx=length(xx)
  tempmat=as.data.frame(matrix(NA,nrow=nx,ncol=3))
  for(i in 1:nx){
    tempmat[i,1]=names(xx)[i]
    tempmat[i,2]=formatC(xx[i], big.mark=",")
    tempmat[i,3]=paste("(",format(xxpercent[i],nsmall=mydigits),")",sep="")
    if(names(xx)[i]!="NA" && print5==F && less5==1){
      if(xx[i]<BCthresh)    {
        tempmat[i,2]="suppressed"
        tempmat[i,3]="suppressed"
      }
    }
  }
  #print(tempmat)
  names(tempmat)=c("Categories","n","%")
  return(tempmat)  #returns the data.frame tempmat
}
