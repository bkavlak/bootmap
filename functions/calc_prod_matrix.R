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

calc_prod_matrix <- function(conf_matrix){
  
  # Calculate Producer Matrix
  prod_matrix <- conf_matrix %>%
    t() %>%
    as.data.frame.matrix() %>%
    purrr::map_if(is.numeric, ~./sum(.)) %>% 
    as_tibble()
  
  # Add Rownames
  prod_matrix$RowName <- colnames(prod_matrix)
  prod_matrix %<>%
    tibble::column_to_rownames(var = "RowName")
  
  return(prod_matrix)
  
}
