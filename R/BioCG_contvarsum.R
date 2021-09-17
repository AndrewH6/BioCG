#' provides a nice summary for a continuous variable, with optional boxplot, histogram

#' @export

BioCG_contvarsum=function(mat, idcol, varcol, doplot=T,myuni=T,check5=F,print5=T) {
  #provides a nice summary for a continuous variable, with optional boxplot, histogram
  #assumes that missing data are NA or "NA", all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #checks to see if any counts are too small to exit CaraSpace; would only be the case if data less than 5 because some of the summaries might also be the exact data
  #mat=matrix or data.frame containing the data
  #idcol=column number of the subject ID
  #varcol=column number of the continuous variable you want to summarize
  #doplot=T if T, produces plots; change to F to not do plots
  #myuni=T if T, provide the number of subjects for the data; if F don't provide any info on the number of subjects
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts

  BCthresh=5 #threshold for checking what is too small to leave CaraSpace

  check5=F
  print5=T

  #A little header and some basic info about data
  cat("\n---------------Variable name: ", dimnames(mat)[[2]][varcol], " column=", varcol, "\n")
  cat("Number of non-missing data points: ",
      length(mat[is.na(mat[, varcol]) == F & mat[, varcol] != "NA", varcol]), "\n")
  if(myuni==T){
    cat("Number of subjects for non-missing:",
        length(unique(mat[is.na(mat[, varcol]) == F & mat[, varcol] != "NA", idcol])), "\n")}

  #check if there is a small number in a count that we need to worry about having it leave CaraSpace
  nomistable=table(mat[, varcol])
  less5=0
  if(check5==T){
    if(nrow(mat)<BCthresh) {
      less5=1   #flag that there is a small number in the data or no variability indicating small sample
      cat("\n\n********* WARNING: A count is less than",BCthresh,"and summary not provided.\n\n")
    }
  }

  if(less5==0 || print5==T){
    cat("Summary:\n")
    print(summary(mat[, varcol]))
    cat("SD:",sd(mat[,varcol],na.rm=T),"\n\n")
    if(doplot==T){
      boxplot(mat[,varcol],ylab=dimnames(mat)[[2]][varcol],main="")
      #have to check if there would be counts that are less than the threshold, not sure if CaraSpace requires these counts to be suppressed too
      tthist=hist(mat[,varcol],main=paste(dimnames(mat)[[2]][varcol],"",sep=""),xlab=dimnames(mat)[[2]][varcol]
                  #,plot=F
                  )
      binmin=tthist$counts
      binmin=binmin[binmin>0]  #remove all the 0s
      if(print5==T || (min(binmin)>=BCthresh)){
        print(tthist)
      }
      else{
        cat("\nNOTE: Histogram not printing because cell size smaller than",BCthresh,". Can see by forcing print5=T.")
      }
    }
  }
}
