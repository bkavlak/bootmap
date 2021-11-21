require(magrittr)
require(ggplot2)

boot_vis_single <- function(
  vis_data,
  plot_name,
  plot_source,
  type = "Area", # or "Accuracy"
  class_list = c("C15"),
  class_decoder = c("Tomato"),
  fill_cols = c("Tomato" = "#e70d46")){
  
  # Create Class Decoder Frame
  class_frame <- data.frame(class_list, class_decoder)
  colnames(class_frame) <- c("ClassId", "ClassName")
  
  # Left Join with class_frame
  vis_data %<>% dplyr::left_join(class_frame, by = "ClassId")
  
  # Filter Desired Classes
  vis_data %<>% dplyr::filter(ClassId %in% class_list)
  
  # Calc Interval Frame & Add ClassName
  interval_frame <- calc_interval_frame(vis_data,
                                        confidence = 0.95,
                                        type = type)
  interval_frame %<>%
    dplyr::left_join(class_frame, by = "ClassId") %>%
    dplyr::select(-ClassId)
  
  # Preapre Legend
  if (type == "Area") {
   
    legend_list <- c(
      paste0("95% CT1: ",
             format(interval_frame$FirstInterval,
                    decimal.mark = ".",
                    big.mark = ",",
                    digits = 2),
             " ha"),
      paste0("      Mean: ",
             format(interval_frame$Mean,
                    decimal.mark = ".",
                    big.mark = ",",
                    digits = 2),
             " ha"),
      paste0("95% CT2: ",
             format(interval_frame$SecondInterval,
                    decimal.mark = ".",
                    big.mark = ",",
                    digits = 2),
             " ha")) 
    
  } else {
    
    legend_list <- c(
      paste0("95% CT1: %",
             format(interval_frame$FirstInterval * 100,
                    decimal.mark = ".",
                    digits = 2)),
      paste0("      Mean: %",
             format(interval_frame$Mean * 100,
                    decimal.mark = ".",
                    digits = 2)),
      paste0("95% CT2: %",
             format(interval_frame$SecondInterval * 100,
                    decimal.mark = ".",
                    digits = 2)))
    
  }
    
  # Create the Plot
  boot_plot <- ggplot(vis_data,
                      aes(x=!!dplyr::sym(type), fill=ClassName)) +
    geom_histogram(color="black", bins = 30) +
    theme(legend.title = element_blank()) +
    #coord_cartesian(ylim = c(0, 250)) +
    geom_vline(data = interval_frame,
               mapping = aes(xintercept = FirstInterval,
                             color = "a_confidence025"),
               show.legend = TRUE, linetype='dashed') +
    geom_vline(data = interval_frame,
               mapping = aes(xintercept = Mean,
                             color = "b_mean"),
               show.legend = TRUE) +
    geom_vline(data = interval_frame,
               mapping = aes(xintercept = SecondInterval,
                             color = "c_confidence975"),
               show.legend = TRUE, linetype='dashed') +
    scale_fill_manual(values = fill_cols) +
    scale_color_manual(name = "Interval Legend",
                       values = c(
                         a_confidence025 = "#B00000",
                         b_mean = "#00008B",
                         c_confidence975 = "#B00000"),
                       labels = legend_list)
  
  # Save Plot
  ggplot2::ggsave(
    filename = paste0(plot_source, "/", plot_name),
    width = 40, height = 30, units = "cm",
    dpi = "retina",
    plot = boot_plot)
  
}