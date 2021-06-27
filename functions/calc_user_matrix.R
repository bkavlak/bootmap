library(dplyr)
library(magrittr)

# It expects a matrix as 
#         Predictions
#         11 12 13 14
#      11
# True 12
#      13
#      14

## ADD CLASS FILTER TO CONFUSION MATRIX

calc_user_matrix <- function(conf_matrix){
  
  # Calculate User Matrix
  user_matrix <- conf_matrix %>%
    purrr::map_if(is.numeric, ~./sum(.)) %>% 
    as_tibble()
  
  # Add Rownames
  user_matrix$RowName <- colnames(user_matrix)
  user_matrix %<>%
    tibble::column_to_rownames(var = "RowName")
  
  return(user_matrix)
  
  }