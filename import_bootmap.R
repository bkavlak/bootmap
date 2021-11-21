import_bootmap <- function(root) {
  
  func_list <- list.files(paste0(root, "/functions"), "*.R$",
                          full.names = TRUE,
                          recursive = TRUE)
  sapply(func_list, source)
  
}