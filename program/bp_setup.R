bp_setup_folder <- function() {
  
  # Trial Version
  version_name <- "v05"
  
  # Repetition List
  repetition_list <- c(50, 100, 200, 500, 1000, 1500)
  
  # Define Polygon Source Directory
  polygon_source <<- "./DATA"
  
  # Define Model Source Directory
  model_source <<- "./Results_RAW/V05/SINGLE"
  
  # Define Polygon Source Directory
  indices_source <<- "./Results_RAW/V05"
  
  # Define Model Source Directory
  accuracy_dest <<- "./Results_RAW/V05"
  
  # Define Bootstrap Histogram Destination
  boot_histo_dest <<- "./Results/V05/Bootstrap_Histogram"
  
  # Define Bootstrap Table Destination
  boot_table_dest <<- "./Results/V05/Bootstrap_Table"
  
  # Area Calculation on the Experiment 45 (Final Decided Map)
  area_matrix <- data.frame(
    area_list = c(2471.446, 539.2134, 890.6652,
                  2396.22, 5463.848, 504.6588,
                  97.4421, 6666.021, 661.9248,
                  322.569, 506.1456, 2335.543,
                  117.3798),
    row.names = c(11, 12, 13, 14,
                  15, 16, 17, 18,
                  19, 21, 22, 23, 24))
  
}