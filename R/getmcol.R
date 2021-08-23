#' determine the (multiple) column number that has the column names

#' @export

getmcol=function(mymat,colvec){
  #given a data.frame mymat, determine the column numbera that have the column names = colvec
  #helpful for going between column names and actual column numbers
  xx=rep(NA,length(colvec))
  for(i in 1:length(colvec)){
    xx[i]=getcol(mymat,colvec[i])
  }
  return(xx)
}
