drop_non_square <- function(conf_matrix) {
  
  if (nrow(conf_matrix) == ncol(conf_matrix)) {
    
    return(conf_matrix)
    
  } else {
    
    return(NA)
    
  }
  
}