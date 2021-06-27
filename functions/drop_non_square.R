drop_non_square <- function(data_df) {
  
  if (nrow(data_df) == ncol(data_df)) {
    
    return(data_df)
    
  } else {
    
    return(NA)
    
  }
  
}