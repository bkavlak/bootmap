# It calculates the matrix frame as 
#         Predictions
#         11 12 13 14
#      11
# True 12
#      13
#      14

boot_confusion <- function(
  test_data,
  boot_indices,
  index_column,
  class_column = "ClassId",
  pred_column = "Predictions") {
  
  boot_indices <- as.data.frame(boot_indices)
  colnames(boot_indices) <- "BootIndex"
  boot_data <- dplyr::left_join(
    boot_indices,
    test_data,
    by = c("BootIndex" = index_column),
    keep = TRUE, copy = TRUE)
  
  # Remove NA
  boot_data <- na.omit(boot_data)
  
  # Get Confusion Matrix
  conf_matrix <- table(boot_data[[class_column]], boot_data[[pred_column]])
  conf_matrix %<>% as.data.frame.matrix()
  
  return(conf_matrix)
}