#' provides a nice summary for two continous variables with a scatterplot with lowess and the correlation

#' @export

bicontcont=function(mat, idcol, xcol, ycol,check5=F,print5=T){
  #provides a nice summary for two continous variables with a scatterplot with lowess and the correlation
  #assumes that missing data are NA or "NA", all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #checks to see if any counts are too small to exit CaraSpace; would only be the case if data less than 5 because some of the summaries might also be the exact data
  #mat=matrix or data.frame containing the data
  #idcol=column number of the subject ID
  #xcol=column number for the variable to be used for the x-axis
  #ycol=column number for the variable to be used for the y-axis
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts

  check5=F
  print5=T

  cat("\n\n******************",names(mat)[xcol],"by",names(mat)[ycol],":\n")
  contvarsum(mat, idcol, xcol, doplot=F,check5,print5)
  contvarsum(mat, idcol, ycol, doplot=F,check5,print5)
  cat("Correlation is ",cor(mat[,xcol],mat[,ycol]),"\n")

  plot(mat[,xcol],mat[,ycol],ylab=names(mat)[ycol],xlab=names(mat)[xcol],main=paste(names(mat)[ycol],"by",names(mat)[xcol],sep=" "))
  lines(lowess(mat[,xcol],mat[,ycol]), col="blue")
}
