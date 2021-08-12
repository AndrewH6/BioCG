#' Name NA to something

#' @export

BCG_cat_Missing <- function(x,the_name = "Missing"){
  old_level <- levels(x)
  x <- as.vector(x)
  x[is.na(x)] <- the_name
  x <- factor(x,
              levels = c(old_level,the_name))
  return(x)
}
