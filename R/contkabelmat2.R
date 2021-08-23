#' Not needed likely as the calcs aren't so ownerous that we cant just use contkabelmat

#' @export

contkabelmat2=function(y,mylab,decimals=1){
  #Not needed likely as the calcs aren't so ownerous that we cant just use contkabelmat
  #mylab is a label to print out to keep with the results, ie could be a nice name for y
  #decimals is the number of decimals to print out
  #returns a matrix of counts and %

  #similar to above but the calculations are already done so just need to arrange into the right positions
  meanxx=format(round(y$mean,decimals),nsmall=decimals);sdxx=format(round(y$sd,decimals),nsmall=decimals);
  medianxx=format(round(y$median,decimals),nsmall=decimals);p25xx=format(round(y$Q1,decimals),nsmall=decimals);  p75xx=format(round(y$Q3,decimals),nsmall=decimals)
  minxx=format(round(y$min,decimals),nsmall=decimals);maxxx=format(round(y$max,decimals),nsmall=decimals); missingxx=y$n.miss
  mymat=as.data.frame(matrix(NA,nrow=4,ncol=4))
  mymat[,1]=mylab
  mymat[1,2]="mean (SD)";mymat[1,3]=meanxx;mymat[1,4]=sdxx #paste("(",sdxx,")",sep="")
  mymat[2,2]="median [25%ile, 75%ile]";
  mymat[2,3]=medianxx;
  mymat[2,4]=paste("[",p25xx,"\ ",p75xx,"]",sep="")
  mymat[3,2]="min, max";mymat[3,3]=minxx;mymat[3,4]=maxxx
  mymat[4,2]="NA";mymat[4,3]=missingxx;mymat[4,4]=format(round(100*missingxx/y$n,decimals),nsmall=decimals)
  names(mymat)=c("C0","C1","C2","C3")
  #return(mymat)
}
