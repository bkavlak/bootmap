library(rsample)
library(furrr)
library(sf)
library(modelr)
library(tidyverse)
library(dplyr)
library(tidymodels)
library(purrr)
library(ggplot2)
library(magrittr)
library(data.table)
library(egg)
library(grid)
library(extrafont)
library(profvis)
library(webshot)
library(gt)
library(here)

# Setup Folder & Load Functions
repo_source <<- here()
source(paste0(repo_source, "/setup.R"))
source(paste0(repo_source, "/import_bootmap.R"))
import_bootmap(repo_source)
setup_folder()

# Get data
polygons_sf <- sf::st_read(paste0(repo_source, "/data/", polygon_name))
test_df <- polygons_sf %>%
  dplyr::select(all_of(c(polygonid_column, classid_column))) %>%
  sf::st_drop_geometry()

# Plan multi-process
future::plan("multisession", gc = TRUE, workers = 5)

# Get model output
model_result_df <- readRDS(paste0(repo_source, "/data/", model_result))

# PRODUCE INDICES ---------------------------------------------------------

# Bootstrap polygons for different repetition number
for (rep_number in repetition_list) {
  
  cat(paste0("Bootstrap with ", rep_number, " repetitions is started."), "\n")
  
  # Sample indices for given repetition number
  polygon_indices_bags <- furrr::future_map(
    1:rep_number,
    ~ boot_sample(
      test_data = test_df,
      sample_size = nrow(test_df),
      polid_column = polygonid_column,
      class_column = classid_column),
    .options = furrr::furrr_options(seed = 1773))
  
  cat(paste0("Bootstrap with", rep_number, " repetitions is finished."), "\n")
  
  # Save as object for later use
  saveRDS(polygon_indices_bags,
          paste0(repo_source,
                 "/computed_lists/polygon_indices_bags_rep", rep_number, ".rds"))
  
}

# PRODUCE ACCURACY BAGS --------------------------------------------------

# Simulate Confusion Matrix List
for (rep_number in repetition_list){
  
  # Read Bootstrap Indices with Target Repetition Number
  polygon_indices_bags <- readRDS(
    paste0(repo_source,
           "/computed_lists/polygon_indices_bags_rep", rep_number, ".rds"))
  
  cat(paste0("Bootstrap with ", rep_number, " repetitions is started."), "\n")
  
  # Calculate Confusion Matrix for each bag
  map_accuracy_bags <- polygon_indices_bags %>%
    furrr::future_map(
      ~ boot_confusion(
        test_data = model_result_df,
        boot_indices = .x,
        index_column = polygonid_column,
        class_column = classid_column,
        pred_column = prediction_column)) %>%
    furrr::future_map(
      ~ drop_non_square( # Convert Non square confusion matrices to NA
        # Controls are added here to prevent non square
        # confusion matrices, but may still result somehow.
        conf_matrix = .x)
    )
  map_accuracy_bags <- map_accuracy_bags[!is.na(map_accuracy_bags)] # Drop NAs
  
  cat(paste0("Bootstrap with ", rep_number, " repetitions is finished."), "\n")
  
  saveRDS(map_accuracy_bags,
          paste0(repo_source,
                 "/computed_lists/map_accuracy_bags_rep", rep_number, ".rds"))
}

# PRODUCE AREA ADJUSTED BAGS ---------------------------------------------------

# Calculate Simulated Adjusted Area
for (rep_number in repetition_list){
  
  # Read Boot Indices with Target Number
  map_accuracy_bags <- readRDS(
    paste0(repo_source,
           "/computed_lists/map_accuracy_bags_rep", rep_number, ".rds"))
  
  cat(paste0("Bootstrap with ", rep_number, " repetitions is started."), "\n")
  
  # Adjust area in bags
  map_area_bags <- map_accuracy_bags %>%
    furrr::future_map(
      ~ adjust_area(
        conf_matrix = .x,
        area_matrix = area_matrix,
        type = "User"))
  
  cat(paste0("Bootstrap with ", rep_number, " repetitions is finished."), "\n")
  
  saveRDS(map_area_bags,
          paste0(repo_source,
                 "/computed_lists/map_area_bags_rep", rep_number, ".rds"))
}


# COMPARE REPETITIONS -----------------------------------------------------

# Prepare an empty list to iterate
map_accuracy_bags_list <- vector("list", 6)

for (i in seq_along(repetition_list)) {
  # Read Accuracy Bags
  map_accuracy_bags <- readRDS(
    paste0(repo_source, "/computed_lists/map_accuracy_bags_rep",
           repetition_list[[i]], ".rds"))
  # Prepare Data to Visualization
  rep_accuracy_data <- map_accuracy_bags %>%
    prepare_accuracy_vis()
  # Add Repetition Indicator
  rep_accuracy_data$RepetitionNumber <- repetition_list[[i]]
  # Add Data to Accuracy List
  map_accuracy_bags_list[[i]] <- rep_accuracy_data
}

# Bind Rows
map_accuracy_df <- map_accuracy_bags_list %>%
  dplyr::bind_rows() %>%
  dplyr::filter(ClassId == "C15") %>%
  dplyr::mutate(ClassName := "Tomato")

# Add "n = " to the Repetition Number
map_accuracy_df %<>% 
  mutate(RepetitionNumber = paste0("n = ", RepetitionNumber))
map_accuracy_df$RepetitionNumber <- factor(
  map_accuracy_df$RepetitionNumber,
  levels = c(
    "n = 50", "n = 100", "n = 200", "n = 500",
    "n = 1000", "n = 1500")
)

# Create the Plot
rep_plot <- ggplot(
  map_accuracy_df,
  aes(x=Accuracy, fill=ClassName)) +
  theme(legend.title = element_blank()) +
  geom_histogram(color="black", bins = 30) +
  scale_fill_manual(values = c("Tomato"="#FF0000")) +
  facet_wrap(. ~ RepetitionNumber) +
  theme(text = element_text(size = 22)) +
  theme(legend.position = "none")

# Save Plot
ggplot2::ggsave(
  filename = paste0(repo_source, "/visualizations",
                    "/bootmap_accuracyhistogram_differentreps_tomato.png"),
  width = 40, height = 30, units = "cm",
  dpi = "retina",
  plot = rep_plot)

# ALLUVIAL PLOT -----------------------------------------------------------

p <- alluvial_plot(
  test_df = model_result_df,
  prediction_column = prediction_column,
  reference_column = classid_column,
  class_id = class_id,
  class_decoder = class_decoder,
  class_color = class_color
)

render_function_str <- build_htmlrender_str(
  class_decoder = color_df$class_decoder,
  class_color = color_df$class_color
)
p <- htmlwidgets::onRender(p, eval(render_function_str))


# Save Plot
htmlwidgets::saveWidget(
  p, file = paste0(repo_source,
                   "/visualizations/bootmap_alluvialplot_test_allclasses.html")
)

# BOOTSTRAP HISTOGRAM -----------------------------------------------------

# Work on 1500 repetitions
# Read Accuracy Bags
map_accuracy_bags <- readRDS(
  paste0(repo_source,
         "/computed_lists/map_accuracy_bags_rep1500.rds"))

# Prepare Accuracy Bags for visualization
accuracy_data <- map_accuracy_bags %>% prepare_accuracy_vis()

# Visualize Accuracy Histogram - Multiple Classes
boot_vis_multiple(
  vis_data = accuracy_data,
  plot_name = "bootmap_accuracyhistogram_rep1500_allclasses.png",
  plot_source = paste0(repo_source, "/visualizations"),
  type = "Accuracy",
  class_list = class_list,
  class_decoder = class_decoder,
  fill_cols = class_color
)

# Visualize Accuracy Histogram - Single Class
for (class_id in class_list) {
  
  # Filter class from class_df
  class_scope_df <- class_df %>%
    dplyr::filter(class_list == class_id)
  
  # Visualize
  boot_vis_single(
    vis_data = accuracy_data,
    plot_name = paste0(
      "bootmap_accuracyhistogram_rep1500_",
      class_scope_df$class_decoder,
      ".png"),
    plot_source = paste0(repo_source, "/visualizations"),
    type = "Accuracy",
    class_list = class_id,
    class_decoder = class_scope_df$class_decoder,
    fill_cols = class_scope_df$class_color
  )
  
}

# Read Area Bags
map_area_bags <- readRDS(
  paste0(repo_source,
         "/computed_lists/map_area_bags_rep1500.rds"))

# Prepare Area Bags for visualization
area_data <- map_area_bags %>% prepare_area_vis()

# Visualize Area Histogram - Multiple Classes
boot_vis_multiple(
  vis_data = area_data,
  plot_name = "bootmap_areahistogram_rep1500_allclasses.png",
  plot_source = paste0(repo_source, "/visualizations"),
  type = "Area",
  class_list = class_list,
  class_decoder = class_decoder,
  fill_cols = class_color
)

# Visualize Area Histogram - Single Class
for (class_id in class_list) {
  
  # Filter class from class_df
  class_scope_df <- class_df %>%
    dplyr::filter(class_list == class_id)
  
  # Visualize
  boot_vis_single(
    vis_data = area_data,
    plot_name = paste0(
      "bootmap_areahistogram_rep1500_",
      class_scope_df$class_decoder, ".png"),
    plot_source = paste0(repo_source, "/visualizations"),
    type = "Area",
    class_list = class_id,
    class_decoder = class_scope_df$class_decoder,
    fill_cols = class_scope_df$class_color
  )
  
}

# BOOTSTRAP TABLE ---------------------------------------------------------

# Work on 1500 repetitions
# Read Accuracy Bags
map_accuracy_bags <- readRDS(
  paste0(repo_source,
         "/computed_lists/map_accuracy_bags_rep1500.rds"))

# Accuracy Table
accuracy_table <- prepare_list_table(
  bag_list = map_accuracy_bags,
  type = "Accuracy")

boot_vis_table(
  table_data = accuracy_table,
  plot_name = "bootmap_accuracytable_rep1500_allclasses.png",
  plot_source = paste0(repo_source, "/visualizations"),
  type = "Accuracy",
  class_list = class_list,
  class_decoder = class_decoder,
  unit = "%")

# Read Area Bags
map_area_bags <- readRDS(
  paste0(repo_source,
         "/computed_lists/map_area_bags_rep1500.rds"))

# Area Table
area_table <- prepare_list_table(
  bag_list = map_area_bags,
  type = "Area")

# Add First Area Estimation Column
area_table$AreaEstimation <- area_matrix$area_list

boot_vis_table(
  table_data = area_table,
  plot_name = "bootmap_areatable_rep1500_allclasses.png",
  plot_source = paste0(repo_source, "/visualizations"),
  type = "Area",
  class_list = class_list,
  class_decoder = class_decoder,
  unit = "ha")
