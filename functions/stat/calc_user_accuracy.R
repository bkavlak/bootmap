calc_user_accuracy <- function(conf_matrix) {
  
  # Calculate User Matrix
  user_matrix <- conf_matrix %>% calc_user_matrix()
  
  # Extract User Accuracy
  user_accuracy <- user_matrix %>%
    as.matrix %>%
    diag()
  
  return(user_accuracy)
  
}