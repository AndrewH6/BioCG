#' returns a data.frame of summary statistics on a time variable y that is in fractional hours and converts to HH:MM; good for exporting/usine elsewhere

#' @export

conttablehm=function(y,mydigits=1){
  #returns a data.frame of summary statistics on a time variable y that is in fractional hours and converts to HH:MM; good for exporting/usine elsewhere
  #assumes that missing data are NA, all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #doesn't check to see if any counts are too small to exit CaraSpace because these are continuous variables and unless so few, wouldn't show individual values
  #y=vector containing the data in hours (fractional)
  #mydigits=number of decimal places to display in the table
  #print(summary(y))
  meanxx=format(as.POSIXct(Sys.Date() + mean(y,na.rm =T)/24), "%Hh %Mm", tz="UTC")
  sdxx=format(as.POSIXct(Sys.Date() +sqrt(var(y,na.rm=T))/24), "%Hh %Mm", tz="UTC")
  medianxx=format(as.POSIXct(Sys.Date() + quantile(y,0.5,na.rm=T)/24),"%Hh %Mm", tz="UTC")
  p25xx=format(as.POSIXct(Sys.Date() + quantile(y,0.25,na.rm=T)/24),"%Hh %Mm", tz="UTC")
  p75xx=format(as.POSIXct(Sys.Date() + quantile(y,0.75,na.rm=T)/24), "%Hh %Mm", tz="UTC")
  missingxx=length(y[is.na(y)])
  tempmat=as.data.frame(matrix(NA,nrow=3,ncol=3))
  tempmat[1,1]="mean (SD)";tempmat[1,2]=meanxx;tempmat[1,3]=paste("(",sdxx,")",sep="")
  tempmat[2,1]="median [25%ile,75%ile]";tempmat[2,2]=medianxx;
  tempmat[2,3]=paste("[",p25xx,",",p75xx,"]",sep="")
  tempmat[3,1]="missing";tempmat[3,2]=missingxx
  #print(tempmat)
  tempmat[is.na(tempmat)]=""
  return(tempmat)
}
