setup_folder <- function() {
  
  # Repetition List
  repetition_list <<- c(50, 100, 200, 500, 1000, 1500)
  
  # Define Model Source Directory
  polygon_name <<- "test_data.shp"
  model_result <<- "classification_result_ontest.rds"
  
  # Attributes
  polygonid_column <<- "PolygonId"
  classid_column <<- "ClassId"
  prediction_column <<- "Predictions"
  
  # Area Calculation on the Map
  area_matrix <<- data.frame(
    area_list = c(2471.446, 539.2134, 890.6652,
                  2396.22, 5463.848, 504.6588,
                  97.4421, 6666.021, 661.9248,
                  322.569, 506.1456, 2335.543,
                  117.3798),
    row.names = c(11, 12, 13, 14,
                  15, 16, 17, 18,
                  19, 21, 22, 23,
                  24))
  
  # Visualization Lists - Dropping Water Class for visual purposes
  class_id <<- c(12, 13, 14,
                 15, 16, 17,
                 18, 19, 21)
  
  class_decoder <<- c("Sunflower", "Capia Pepper", "Paddy",
                      "Tomato", "Watermelon", "Melon",
                      "Corn", "Orchard", "Alfalfa")
  
  class_color <<- c("#0044FF", "#4BFF00", "#00FFF2",
                    "#FF0000", "#107717", "#B1BC00",
                    "#F0FF00", "#720E91", "#46FFA1")
  
  # Visualization on Integer values is sometimes problematic.
  # We are adding "C" to work properly on bootstrap visualizations
  class_list <<- paste0("C", class_id)
  
  
  # Prepare Class DF
  class_df <<- data.frame(class_list, class_decoder, class_color,
                          stringsAsFactors = FALSE)
  
  # Prepare Color DF
  color_df <- data.frame(class_decoder, class_color)
  color_df <<- dplyr::arrange(color_df, class_decoder)
  
  
}