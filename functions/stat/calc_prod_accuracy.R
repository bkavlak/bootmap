calc_prod_accuracy <- function(conf_matrix) {
  
  # Calculate Producer Matrix
  prod_matrix <- conf_matrix %>% calc_prod_matrix()
  
  # Extract Producer Accuracy
  prod_accuracy <- prod_matrix %>%
    as.matrix %>%
    diag()
  
  return(prod_accuracy)
  
}