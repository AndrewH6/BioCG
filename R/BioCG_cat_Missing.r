#' Name NA to something

#' @export

BioCG_cat_Missing <- function(x,the_name = "Missing"){

  if(!is.null(levels(x))){
    old_level <- levels(x)
    x <- as.vector(x)
    x[is.na(x)] <- the_name
    x <- factor(x,levels = c(old_level,the_name))}
  else if(is.null(levels(x))){
    x <- as.vector(x)
    x[is.na(x)] <- the_name
  }

  return(x)
}
