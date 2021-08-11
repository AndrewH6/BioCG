#' Welcome people

#' @export

BioCG_welcome = function(name){
  t = seq(0,60,len=100)
  plot(c(-8,8),c(0,20),type='n',axes=FALSE,xlab='',ylab='')
  x = -.01*(-t^2+40*t+1200)*sin(pi*t/180)
  y = .01*(-t^2+40*t+1200)*cos(pi*t/180)
  lines(x,y, lwd=4,col='red')
  lines(-x,y, lwd=4,col='red')
  text(0,7,"Welcome to BioCG!",col='red',cex=1.5)
  text(0,5.5,name,col='red',cex=2.5)
}
