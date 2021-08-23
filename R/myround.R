#' print out the value of x with the number of digits = digits

#' @export

myround=function(x,digits=3){
  #print out the value of x with the number of digits = digits, the default is 3 if no digits= argument supplied
  #forces 0s to be printed so that round(1.420,digits=3) becomes 1.420 and not 1.42
  y=format(round(x,digits),nsmall=digits)
  return(y)}
