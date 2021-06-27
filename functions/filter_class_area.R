library(magrittr)
library(dplyr)

filter_class_area <- function(data, class_id) {
  
  data %<>% dplyr::filter(ClassId == class_id)
  data <- data$Area
  
  return(data)
  
}