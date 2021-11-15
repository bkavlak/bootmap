library(gt)
library(glue)
library(tidyverse)

boot_table_vis <- function(table_data,
                           plot_name,
                           plot_source,
                           type = "Area", # or "Accuracy"
                           class_list = c("C11", "C12", "C13", "C14",
                                          "C15", "C16", "C17", "C18",
                                          "C19", "C21", "C22", "C23", "C24"),
                           class_decoder = c("Soil", "Sunflower", "Pepper", "Rice",
                                             "Tomato", "Watermelon", "Melon", "Corn",
                                             "Sugarbeet", "Alfalfa", "Orchard",
                                             "Vegetation", "Road")) {
  
  
  
  # Create Class Decoder Frame
  class_frame <- data.frame(class_list, class_decoder)
  colnames(class_frame) <- c("ClassId", "ClassName")
  
  # Left Join with class_frame
  table_data %<>% dplyr::left_join(class_frame, by = "ClassId")
  
  # Filter Desired Classes
  table_data %<>% dplyr::filter(ClassId %in% class_list)
  
  if(type == "Accuracy") {
    
    table_data %<>%
      dplyr::select(-ClassId) %>%
      gt::gt() %>%
      gt::tab_header(
        title = "Bootstrap Accuracy Table",
        subtitle = "User & Producer Accuracy Confidence Intervals"
      ) %>%
      gt::tab_spanner(
        label = "User Accuracy",
        columns = c(UserFirstInterval, UserMeanAccuracy, UserMedianAccuracy, UserSecondInterval)
      ) %>%
      gt::tab_spanner(
        label = "Producer Accuracy",
        columns = c(ProdFirstInterval, ProdMeanAccuracy, ProdMedianAccuracy, ProdSecondInterval)
      ) %>%
      gt::cols_move_to_start(
        columns = c(ClassName, UserFirstInterval, UserMeanAccuracy, UserMedianAccuracy, UserSecondInterval)
      ) %>%
      gt::cols_label(
        ClassName = "Class Name",
        UserFirstInterval = "Lower Threshold CI %95",
        UserMeanAccuracy = "Mean Accuracy",
        UserMedianAccuracy = "Median Accuracy",
        UserSecondInterval = "Upper Threshold CI %95",
        ProdFirstInterval = "Lower Threshold CI %95",
        ProdMeanAccuracy = "Mean Accuracy",
        ProdMedianAccuracy = "Median Accuracy",
        ProdSecondInterval = "Upper Threshold CI %95",
      )
    
    # Save plot
    gt::gtsave(table_data, paste0(plot_source, "/", plot_name))
    
    
  } else if (type == "Area") {

    table_data %<>%
      dplyr::select(-ClassId) %>%
      gt::gt() %>%
      gt::tab_header(
        title = "Bootstrap Area Adjustment Table",
        subtitle = "User Area Confidence Intervals"
      ) %>%
      gt::tab_spanner(
        label = "Adjustment with User Accuracy",
        columns = c(UserFirstInterval, UserMeanArea, UserMedianArea, UserSecondInterval)
      ) %>%
      gt::cols_move_to_start(
        columns = c(ClassName, AreaEstimation, UserFirstInterval, UserMeanArea, UserMedianArea, UserSecondInterval)
      ) %>%
      gt::cols_label(
        ClassName = "Class Name",
        AreaEstimation = "Area Estimation",
        UserFirstInterval = "Lower Threshold CI %95",
        UserMeanArea = "Mean Area Adjustment",
        UserMedianArea = "Median Area Adjustment",
        UserSecondInterval = "Upper Threshold CI %95",
      )
    
    # Save plot
    gt::gtsave(table_data, paste0(plot_source, "/", plot_name))
  }
  
}

