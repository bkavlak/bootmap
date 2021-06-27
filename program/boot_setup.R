boot_setup_folder <- function() {
  
  # Define Polygon Source Directory
  polygon_source <<- ".../DATA"
  
  # Define Model Source Directory
  model_source <<- ".../Results_RAW/V05/SINGLE"
  
  # Define Polygon Source Directory
  indices_source <<- ".../Results_RAW/V05"
  
  # Define Model Source Directory
  accuracy_dest <<- ".../Results_RAW/V05"
  
  # Define Bootstrap Histogram Destination
  boot_histo_dest <<- ".../Results/V05/Bootstrap_Histogram"
  
  # Define Bootstrap Table Destination
  boot_table_dest <<- ".../Results/V05/Bootstrap_Table"
  
}