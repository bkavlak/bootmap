prepare_area_vis <- function(area_list){
  
  # Merge list by rows
  area_frame <- area_list %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  
  # Get ClassId
  class_list <- area_frame %>%
    dplyr::slice(1) %>%
    unlist(use.names = FALSE)
  # Convert Factor to Numeric
  class_list <- as.numeric(levels(class_list))[class_list]
  # Prepare Class Column Names
  class_cols <- paste0("C", class_list)
  
  # Detect Area Rows
  area_frame %<>%
    dplyr::filter(stringr::str_detect(row.names(.),
                                      pattern = "Area*")) %>%
    tibble::remove_rownames()
  # Convert Colnames
  colnames(area_frame) <- class_cols
  
  # Add Simulation Column
  sim_cols <- paste0("S", 1:nrow(area_frame)) 
  area_frame$SimulationIndex <- sim_cols
  
  # Melt to Long
  area_frame %<>%
    tidyr::pivot_longer(cols = all_of(class_cols),
                        names_to = "ClassId",
                        values_to = "Area")
  # Convert Area Factor to Numeric
  area_frame$Area <- as.numeric(levels(area_frame$Area))[area_frame$Area]
  
  return(area_frame)
}