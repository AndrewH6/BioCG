#' print the summary table for category variables

#' @export

printkable<-function(mat,mycap=""){
  #print the summary table for category variables
  #provide a knitr::kable table for the matrix/dataframe mat with the caption mycap
  myalign='lrr'
  if(!is.null(mat)){#can only print out if I have something to print
    knitr::kable(mat, align=myalign,row.names=F,caption=mycap) }
  #kable_styling(bootstrap_options = "striped", full_width = F)
  #row_spec(0, bold = T,background="darkgrey")
}
