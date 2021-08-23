#' provides a nice summary for time to event variable, with optional Kaplan-Meier curve

#' @export


time2eventvarsum=function(mat, idcol, varcol, cencol,doplot=T) {
  #provides a nice summary for time to event variable, with optional Kaplan-Meier curve
  #Doesn't provide a check for CaraSpace
  #mat=matrix or data.frame containing the data
  #idcol=column number of the subject ID
  #varcol=column number of the time variable
  #cencol=column number of the censoring indicator
  #doplot=T if T, produces plots; change to F to not do plots

  cat("\n---------------Variable name: ", dimnames(mat)[[2]][varcol], "column=", varcol, "; Censoring name:",
      dimnames(mat)[[2]][cencol], " column=", cencol,"\n")
  cat("Number of non-missing data points: ",
      length(mat[is.na(mat[, varcol]) == F & mat[, varcol] != "NA", varcol]), "\n")
  cat("Number of subjects for non-missing:",
      length(unique(mat[is.na(mat[, varcol]) == F & mat[, varcol] != "NA", idcol])), "\n")
  cat("Censoring indicator:")
  print(table(mat[,cencol],useNA="ifany"))
  fit=survfit(Surv(mat[,varcol],mat[,cencol])~1)
  if(length(unique(mat[,idcol]))<nrow(mat)){#clustered by ID
    fit=survfit(Surv(mat[,varcol],mat[,cencol])~1+cluster(mat[,idcol]))
    cat("*********MODEL WITH CLUSTER for ID\n")
  }
  print((fit))
  plot(fit,xlab="Days",ylab="Proportion without event",main=dimnames(mat)[[2]][varcol])
  cat("\n")
}
