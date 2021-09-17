#' summarize cat and cont variable

#' @export


BioCG_catcontvarsum <- function(data_mat,var_name,cat_var=TRUE){

data_mat <- data.frame(data_mat)

the_var_index <- getcol(data_mat,var_name)

if(cat_var == TRUE){
  BioCG::BioCG_catvarsum(data_mat,the_var_index,the_var_index)
} else if(cat_var == FALSE){
  BioCG::BioCG_contvarsum(data_mat,the_var_index,the_var_index)
}

}
