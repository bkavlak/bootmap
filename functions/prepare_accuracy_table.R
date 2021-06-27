prepare_accuracy_table <- function(conf_matrix_list,
                                   type = "User" # or "Producer"
){
  
  if (type == "User") {
    
    accuracy_frame_list <- conf_matrix_list %>%
      purrr::map(~ calc_user_accuracy(.x))
    accuracy_frame <- accuracy_frame_list %>%
      dplyr::bind_rows()
    
    
  } else if (type == "Producer") {
    
    accuracy_frame_list <- conf_matrix_list %>%
      purrr::map(~ calc_prod_accuracy(.x))
    accuracy_frame <- accuracy_frame_list %>%
      dplyr::bind_rows()
    
  }
  
  # Get Class List
  class_list <- colnames(accuracy_frame)
  
  # Prepare Class Column Names
  class_cols <- paste0("C", class_list)
  
  # Convert Colnames
  colnames(accuracy_frame) <- class_cols
  
  # Add Simulation Column
  sim_cols <- paste0("S", 1:nrow(accuracy_frame)) 
  accuracy_frame$SimulationIndex <- sim_cols
  
  # Melt to Long
  accuracy_frame %<>%
    tidyr::pivot_longer(cols = all_of(class_cols),
                        names_to = "ClassId",
                        values_to = "Accuracy")
  
  return(accuracy_frame)
}