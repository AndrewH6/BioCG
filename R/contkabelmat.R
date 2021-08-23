#' Provides a nice kabel table to summarize a continuous variable supplied in y

#' @export

contkabelmat=function(y,mylab,decimals=1){#print out values
  #Provides a nice kabel table to summarize a continuous variable supplied in y
  #mylab is a label to print out to keep with the results, ie could be a nice name for y
  #decimals is the number of decimals to print out

  #xx=summary(y)
  meanxx=format(round(mean(y,na.rm=T),decimals),nsmall=decimals)
  sdxx=format(round(sqrt(var(y,na.rm=T)),decimals),nsmall=decimals)
  medianxx=format(round(quantile(y,0.5,na.rm=T),decimals),nsmall=decimals)
  p25xx=format(round(quantile(y,0.25,na.rm=T),decimals),nsmall=decimals)
  p75xx=format(round(quantile(y,0.75,na.rm=T),decimals),nsmall=decimals)
  minxx=format(round(min(y,na.rm =T),decimals),nsmall=decimals)
  maxxx=format(round(max(y,na.rm=T),decimals),nsmall=decimals)
  missingxx=length(y[is.na(y)])
  mymat=as.data.frame(matrix(NA,nrow=4,ncol=4))
  mymat[,1]=mylab
  mymat[1,2]="mean (SD)";mymat[1,3]=meanxx;mymat[1,4]=sdxx #paste("(",sdxx,")",sep="")
  mymat[2,2]="median [25%ile, 75%ile]";
  mymat[2,3]=medianxx;
  mymat[2,4]=paste("[",p25xx,"\ ",p75xx,"]",sep="")
  mymat[3,2]="min, max";mymat[3,3]=minxx;mymat[3,4]=maxxx
  mymat[4,2]="NA";mymat[4,3]=missingxx;mymat[4,4]=format(round(100*missingxx/length(y),decimals),nsmall=decimals)
  names(mymat)=c("C0","C1","C2","C3")
  return(mymat)
}
