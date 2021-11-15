exp_boot_iterator <- function(boot_list_name,
                              boot_list_source,
                              exp_number,
                              plot_source,
                              boot_str_order,
                              type = "Area", # or "Accuracy"
                              class_list = c("C11", "C12", "C13", "C14",
                                             "C15", "C16", "C17", "C18",
                                             "C19", "C21", "C22", "C23", "C24"),
                              class_decoder = c("Soil", "Sunflower", "Pepper", "Rice",
                                                "Tomato", "Watermelon", "Melon", "Corn",
                                                "Sugarbeet", "Alfalfa", "Orchard",
                                                "Vegetation", "Road"),
                              fill_cols = c("Soil" = "#fb8807",
                                            "Sunflower" = "#094aaa",
                                            "Pepper" = "#f232ee",
                                            "Rice" = "#31a1c4",
                                            "Tomato" = "#e70d46",
                                            "Watermelon" = "#21f429",
                                            "Melon" = "#f8b70c",
                                            "Corn" = "#f8e00d",
                                            "Sugarbeet" = "#b460c5",
                                            "Alfalfa" = "#1dd4ab",
                                            "Orchard" = "#000000",
                                            "Vegetation" = "#122c0d",
                                            "Road" = "#917d7d")
                              ){
  
  # Prepare data for Vis
  if (type == "Area") {
    
    # Get Bootstrap Indices
    vis_data <- readRDS(paste0(boot_list_source, "/", boot_list_name))
    vis_data <- vis_data[[exp_number]]
    
    # Prepare Area Frame for visualization
    vis_data %<>% prepare_area_vis()
    
  } else if (type == "Accuracy") {
    
    # Get Bootstrap Indices
    vis_data <- readRDS(paste0(boot_list_source, "/", boot_list_name))
    vis_data <- vis_data[[exp_number]]
    
    # Prepare Accuracy Frame for visualization
    vis_data %<>% prepare_accuracy_vis()
    
  }
  
  # Detect Bootstrap N
  boot_strings <- boot_list_name %>%
    stringr::str_split("_") %>%
    unlist
  boot_n <- boot_strings[[boot_str_order]]
  
  # Prepare the Plot Name
  plot_name <- paste0("Experiment", exp_number + 18,
                      "_BootHist_", type, "_", boot_n, ".png")
  
  # Plot the data
  boot_vis(vis_data = vis_data,
           plot_name = plot_name,
           plot_source = plot_source,
           type = type,
           class_list = class_list,
           class_decoder = class_decoder,
           fill_cols = fill_cols)
  
}