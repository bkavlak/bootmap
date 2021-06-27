boot_accuracy <- function(data,
                          boot_indices,
                          index_column,
                          class_col = "id",
                          pred_col = "predictions") {
  
  boot_indices <- as.data.frame(boot_indices)
  colnames(boot_indices) <- "BootIndex"
  boot_data <- dplyr::left_join(boot_indices, data, by = c("BootIndex" = index_column),
                        keep = TRUE, copy = TRUE)
  
  # Remove NA
  boot_data <- na.omit(boot_data)
  
  # Get Confusion Matrix
  conf_matrix <- table(boot_data[[class_col]], boot_data[[pred_col]])
  conf_matrix %<>% as.data.frame.matrix()
  
  return(conf_matrix)
}