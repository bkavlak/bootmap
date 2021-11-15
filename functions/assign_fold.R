# DOKTAR GIS - Convert Engine

# This is a function for assigning CV fold number
# to polygons stratifiedly.

# Created on 11/27/2020
# Version 01

require(sf)
require(dplyr)
require(caret)
require(reshape2)
require(stringr)

assign_fold <- function(vector_source = getwd(),
                       vector_name = "vector_v01.gpkg",
                       response_col = "id",
                       k_fold = 5,
                       fold_col = "IndexFold",
                       class_nums = c(11, 12, 13, 14, 15,
                                   16, 17, 18, 19, 21,
                                   22, 23, 24),
                       vector_dest = getwd(),
                       vector_on = "tempName",
                       vector_of = ".gpkg",
                       split_test = TRUE){
  
  # Read the Vector
  vec <- sf::st_read(paste0(vector_source, "/", vector_name))
  vec <- sf::st_zm(vec)
  
  # Iterate over class_nums
  pol_folds <- lapply(class_nums, function(ci) {
    sub            <- vec[vec[[response_col]] == ci,]  
    folds          <- caret::createFolds(sub[[response_col]], k = k_fold)
    names(folds)   <- NULL
    folds          <- reshape2::melt(folds) 
    sub[[fold_col]] <- folds[order(folds$value), "L1"]
    return(sub)
  })
  
  # Bind class_nums together
  train_data <- do.call(rbind, pol_folds)
  
  if (split_test) {
    
    # Mutate Fold 2 to Test
    train_data %<>%
      dplyr::mutate(SplitSet := dplyr::if_else(!!(dplyr::sym(fold_col)) == 2, "TestSet", "TrainSet"))
    
  }
  
  # Increase version
  vector_on <- increase_version(vector_name, extension = vector_of)
  
  # Write assigned polygon to desired destination
  sf::write_sf(train_data,
               paste0(vector_dest, "/", vector_on),
               overwrite = TRUE)
  
  # Return assigned train data
  return(train_data)
}
