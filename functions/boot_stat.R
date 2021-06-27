boot_stat <- function(data) {
  
  first_int     <- sprintf("%.2f", round(quantile(data[,1], probs = 0.025), 2))
  second_int    <- sprintf("%.2f", round(quantile(data[,1], probs = 0.975), 2))
  median_int    <- sprintf("%.2f", round(quantile(data[,1], probs = 0.50), 2))
  mean_int      <- sprintf("%.2f", round(mean(data[,1]), 2))
  
  table <- cbind(first_int, mean_int, median_int, second_int)
  return(table)
}