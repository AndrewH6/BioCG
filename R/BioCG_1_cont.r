#' Summarize one continuous variable

#' @export

BioCG_1_cont <- function(x){
  the_n_missing <- sum(is.na(x))
  the_summary <- summary(x) %>% as.vector()
  the_sd <- sd(x, na.rm = TRUE)

  the_summary_data <- round(c(the_summary,the_sd,the_n_missing),2) %>% matrix(nrow = 1, ncol = 8) %>% data.frame()
  colnames(the_summary_data) <- c("Min","25th percentile", "Median", "Mean", "75th percentile", "Max", "SD", "NAs (n)")

  return(the_summary_data)
}
