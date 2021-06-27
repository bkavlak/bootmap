# Folding Functions
train_fold <- function(data, col, index) {
  
  return(data[data[[col]] != index,])
  
}

val_fold <- function(data, col, index) {
  
  return(data[data[[col]] == index,])
}