boot_vis_multiple <- function(
  vis_data,
  plot_name,
  plot_source,
  type = "Area", # or "Accuracy"
  class_list = c("C11", "C12", "C13", "C14",
                 "C15", "C16", "C17", "C18",
                 "C19", "C21", "C22", "C23"),
  class_decoder = c("Winter Cultivation", "Sunflower", "Capia Pepper", "Paddy",
                    "Tomato", "Watermelon", "Melon", "Corn",
                    "Orchard", "Alfalfa", "Impervious Surface", "Vegetation"),
  fill_cols = c("Winter Cultivation" = "#fb8807",
                "Sunflower" = "#094aaa",
                "Pepper" = "#f232ee",
                "Rice" = "#31a1c4",
                "Tomato" = "#e70d46",
                "Watermelon" = "#21f429",
                "Melon" = "#f8b70c",
                "Corn" = "#f8e00d",
                "Orchard" = "#000000",
                "Alfalfa" = "#1dd4ab",
                "Impervious Surface" = "#ffffff",
                "Vegetation" = "#122c0d"
                )){
  
  # Create Class Decoder Frame
  class_frame <- data.frame(class_list, class_decoder)
  colnames(class_frame) <- c("ClassId", "ClassName")
  
  # Left Join with class_frame
  vis_data %<>% dplyr::left_join(class_frame, by = "ClassId")
  
  # Filter Desired Classes
  vis_data %<>% dplyr::filter(ClassId %in% class_list)
  
  # Create the Plot
  boot_plot <- ggplot(vis_data,
                      aes(x=!!dplyr::sym(type), fill=ClassId)) +
    geom_histogram(color="black", bins = 30) +
    coord_cartesian(ylim = c(0, 600)) +
    scale_fill_manual(values = fill_cols) +
    facet_wrap(. ~ ClassName) +
    theme(text = element_text(size = 22)) +
    theme(legend.position = "none")
  
  # Save Plot
  ggplot2::ggsave(filename = paste0(plot_source, "/", plot_name),
                  width = 40, height = 30, units = "cm",
                  dpi = "retina",
                  plot = boot_plot)
  
}