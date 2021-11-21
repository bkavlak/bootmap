bp_setup_folder <- function() {
  
  # Repetition List
  repetition_list <<- c(50, 100, 200, 500, 1000, 1500)
  
  # Define Model Source Directory
  repo_source <<- "/media/Backup/PLANET/BETUL_TEZ/PAPER"
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
  
  # Visualization Lists
  class_list <<- c("C11", "C12", "C13", "C14",
                   "C15", "C16", "C17", "C18",
                   "C19", "C21", "C22", "C23")
  
  class_decoder <<- c("Winter Cultivation", "Sunflower", "Capia Pepper", "Paddy",
                      "Tomato", "Watermelon", "Melon", "Corn",
                      "Orchard", "Alfalfa", "Impervious Surface",
                      "Vegetation")
  
  class_color <<- c("Winter Cultivation" = "#FFFFFF",
                   "Sunflower" = "#0044FF",
                   "Capia Pepper" = "#4BFF00",
                   "Paddy" = "#00FFF2",
                   "Tomato" = "#FF0000",
                   "Watermelon" = "#107717",
                   "Melon" = "#B1BC00",
                   "Corn" = "#F0FF00",
                   "Orchard" = "#720E91",
                   "Alfalfa" = "#46FFA1",
                   "Impervious Surface" = "#FFFFFF",
                   "Vegetation" = "#FFFFFF")
  
  # Prepare Class DF
  class_df <<- data.frame(class_list, class_decoder, class_color,
                          stringsAsFactors = FALSE)
  
  
}