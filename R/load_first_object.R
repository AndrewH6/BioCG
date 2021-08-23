#' load Rda

#' @export

load_first_object <- function(fname){
  #this function is to help load Rda files easily and assign to the
  #example: partition2=load_first_object(paste(datapath,"part2_reg_2019Mar2.rda",sep="")) would make sure that the rda file created by the save command becomes the object named partition2
  #ie save(tempdata, file = paste(datapath,"part2_reg_2019Mar2.rda",sep="")) would produce the rda from an object called tempdata
  e <- new.env(parent = parent.frame())
  load(fname, e)
  return(e[[ls(e)[1]]])
}
