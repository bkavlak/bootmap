library(dplyr)

# It expect a data with Polygon Id & Classification Id
# as separate two columns
boot_sample <- function (data, n,
                         polid_column = "polygonID",
                         class_column = "id") {
  
  # Get Unique Class Id
  class_origin <- unique(data[[class_column]])
  
  # Sample Randomly
  sampled_data  <- data[sample(nrow(data), size = n, replace = TRUE),]
  
  # Check if unique class of the original and sampled data is equal
  class_sampled <- unique(sampled_data[[class_column]])
  class_equality <- length(class_origin) == length(class_sampled)
  
  # Return sampled data if condition is satisfied
  # Add at least one polygon from missing class if condition fails
  if(class_equality){
    
    return(sampled_data[[polid_column]])
    
  } else {
    
    # Get the difference of two vectors
    missing_class <- dplyr::setdiff(class_origin, class_sampled)
    
    # Create an empty data frame
    missing_class_pol <- data %>%
      dplyr::slice(1:length(missing_class))
    
    # sample one polygon for missing classes
    for (i in 1:length(missing_class)){
     
      class_data <- data %>%
        dplyr::filter(.data[[class_column]] == missing_class[[i]])
      missing_class_pol[i, ]  <- class_data[sample(nrow(class_data), size = 1, replace = TRUE),] 
      
    }
    
    # Drop polygons from max class to match the number of polygons
    
    # Add sampled polygons to sample data
    sampled_data %<>% rbind(missing_class_pol)
    
    return(sampled_data[[polid_column]])
  }
  
}