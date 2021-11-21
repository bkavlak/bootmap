# It expects a matrix as data frame in the below format: 
#         Predictions
#         11 12 13 14
#      11
# True 12
#      13
#      14
  
## ADD CLASS FILTER TO CONFUSION MATRIX

adjust_area <- function(
  conf_matrix,
  area_matrix,
  type = "User") {
  
  if (type == "User") {
    
    # Convert Frames to Matrix
    user_matrix <- calc_user_matrix(conf_matrix) %>% as.matrix()
    area_matrix %<>% as.matrix()
    
    # Dot Product
    adj_area_matrix <- user_matrix %*% area_matrix
    
    # Name Columns & Get Row Names to a column
    colnames(adj_area_matrix) <- "Area"
    adj_area_matrix %<>%
      dplyr::as_data_frame(rownames = "ClassId")
    
    return(adj_area_matrix) 
    
  } else if (type == "Producer") {
    
    # Convert Frames to Matrix
    prod_matrix <- calc_prod_matrix(conf_matrix) %>% as.matrix()
    area_matrix %<>% as.matrix()
    
    # Dot Product
    adj_area_matrix <- prod_matrix %*% area_matrix
    
    # Name Columns & Get Row Names to a column
    colnames(adj_area_matrix) <- "Area"
    adj_area_matrix %<>%
      dplyr::as_data_frame(rownames = "ClassId")
    
    return(adj_area_matrix)
    
  } else {
    
    print("You are not giving my a proper type: User or Producer.")
    print(paste0("Instead you are giving this: ", type))
    
  }
  
}