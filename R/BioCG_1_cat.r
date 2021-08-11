#' Summarize one categorical variable

#' @export

BioCG_1_cat <- function(x,the_var){

the_result_1 <- janitor::tabyl(x) %>%
  dplyr::mutate(percent = paste0(round(percent,3),"%"),
                !!the_var := paste0(n,paste0("(",percent,")")))

the_result_2 <- setNames(data.frame(t(the_result_1[,-1])),the_result_1[,1])

the_result_3 <- the_result_2[the_var,]

return(the_result_3)
}

