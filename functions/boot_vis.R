boot_vis <- function(vis_data,
                     plot_name,
                     plot_source,
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
                                   "Road" = "#917d7d"
                                   )){
  
  # Create Class Decoder Frame
  class_frame <- data.frame(class_list, class_decoder)
  colnames(class_frame) <- c("ClassId", "ClassName")
  
  # Left Join with class_frame
  vis_data %<>% dplyr::left_join(class_frame, by = "ClassId")
  
  # Filter Desired Classes
  vis_data %<>% dplyr::filter(ClassId %in% class_list)
  
  if(type == "Area") {
    
    # Calc Interval Frame & Add ClassName
    interval_frame <- calc_interval_frame(vis_data,
                                          confidence = 0.95,
                                          type = type)
    interval_frame %<>%
      dplyr::left_join(class_frame, by = "ClassId") %>%
      dplyr::select(-ClassId)
    
    # Create the Plot
    boot_plot <- ggplot(vis_data, aes(x=Area, fill=ClassName)) +
      geom_histogram(color="black", bins = 30) +
      coord_cartesian(ylim = c(0, 600)) +
      # geom_vline(data = interval_frame, mapping = aes(xintercept = FirstInterval, 
      #                                                 color = "a_confidence025"),
      #            show.legend = TRUE, linetype='dashed') +
      # geom_vline(data = interval_frame, mapping = aes(xintercept = Mean, 
      #                                                 color = "b_mean"),
      #            show.legend = TRUE) +
      # geom_vline(data = interval_frame, mapping = aes(xintercept = SecondInterval, 
      #                                                 color = "c_confidence975"),
      #            show.legend = TRUE, linetype='dashed') +
      scale_fill_manual(values = fill_cols) +
      # scale_color_manual(name = "Legend",
      #                    values = c(a_confidence025 = "#B00000",
      #                               b_mean = "#00008B",
      #                               c_confidence975 = "#B00000"),
      #                    labels = c("95% CT1",
      #                               "     Mean",
      #                               "95% CT2")) +
      facet_wrap(. ~ ClassName)
    
    # Save Plot
    ggplot2::ggsave(filename = paste0(plot_source, "/", plot_name),
                    width = 40, height = 30, units = "cm",
                    dpi = "retina",
                    plot = boot_plot)
    
  } else if (type == "Accuracy") {
    
    # Calc Interval Frame & Add ClassName
    interval_frame <- calc_interval_frame(vis_data,
                                          confidence = 0.95,
                                          type = type)
    interval_frame %<>%
      dplyr::left_join(class_frame, by = "ClassId") %>%
      dplyr::select(-ClassId)
    
    # Create the Plot
    boot_plot <- ggplot(vis_data, aes(x=Accuracy, fill=ClassName)) +
      geom_histogram(color="black", bins = 30) +
      coord_cartesian(ylim = c(0, 600)) +
      # geom_vline(data = interval_frame, mapping = aes(xintercept = FirstInterval, 
      #                                                 color = "a_confidence025"),
      #            show.legend = TRUE, linetype='dashed') +
      # geom_vline(data = interval_frame, mapping = aes(xintercept = Mean, 
      #                                                 color = "b_mean"),
      #            show.legend = TRUE) +
      # geom_vline(data = interval_frame, mapping = aes(xintercept = SecondInterval, 
      #                                                 color = "c_confidence975"),
      #            show.legend = TRUE, linetype='dashed') +
      scale_fill_manual(values = fill_cols) +
      # scale_color_manual(name = "Legend",
      #                    values = c(a_confidence025 = "#B00000",
      #                               b_mean = "#00008B",
      #                               c_confidence975 = "#B00000"),
      #                    labels = c("95% CT1",
      #                               "     Mean",
      #                               "95% CT2")) +
      facet_wrap(. ~ ClassName)
    
    # Save Plot
    ggplot2::ggsave(filename = paste0(plot_source, "/", plot_name),
                    width = 40, height = 30, units = "cm",
                    dpi = "retina",
                    plot = boot_plot)
    
  }
  
}