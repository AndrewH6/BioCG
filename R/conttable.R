#' returns a data.frame of summary statistics on continuous vector y that is good for exporting/using elsewhere

#' @export

conttable=function(y,mydigits=1){
  #returns a data.frame of summary statistics on continuous vector y that is good for exporting/using elsewhere
  #assumes that missing data are NA, all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #doesn't check to see if any counts are too small to exit CaraSpace because these are continuous variables and unless so few, wouldn't show individual values
  #y=vector containing the data
  #mydigits=number of decimal places to display in the table

  #print(summary(y))
  meanxx=format(round(mean(y,na.rm =T),mydigits),nsmall=mydigits)
  sdxx=format(round(sqrt(var(y,na.rm=T)),mydigits),nsmall=mydigits)
  minxx=format(round(min(y,na.rm =T),mydigits),nsmall=mydigits)
  maxxx=format(round(max(y,na.rm=T),mydigits),nsmall=mydigits)
  medianxx=format(round(quantile(y,0.5,na.rm=T),mydigits),nsmall=mydigits)
  p25xx=format(round(quantile(y,0.25,na.rm=T),mydigits),nsmall=mydigits)
  p75xx=format(round(quantile(y,0.75,na.rm=T),mydigits),nsmall=mydigits)
  missingxx=length(y[is.na(y)])
  tempmat=as.data.frame(matrix(NA,nrow=4,ncol=3))
  tempmat[1,1]="mean (SD)";tempmat[1,2]=meanxx;tempmat[1,3]=sdxx #paste("(",sdxx,")",sep="")
  tempmat[2,1]="median [25%ile,75%ile]";tempmat[2,2]=medianxx;
  tempmat[2,3]=paste("[",p25xx,",",p75xx,"]",sep="")
  tempmat[3,1]="min, max";tempmat[3,2]=minxx;tempmat[3,3]=maxxx
  tempmat[4,1]="missing";tempmat[4,2]=missingxx
  #print(tempmat)
  tempmat[is.na(tempmat)]=""
  names(tempmat)=c("Measures","","")
  return(tempmat) #returns the data.frame tempmat
}
