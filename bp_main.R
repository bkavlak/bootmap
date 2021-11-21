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

# Load Functions
source(paste0(repo_source, "/bp_setup.R"))
source(paste0(repo_source, "/import_bootmap.R"))
import_bootmap(repo_source)

# Setup Folders
bp_setup_folder()

# Get data
polygons_sf <- sf::st_read(paste0(repo_source, "/", polygon_name))
test_df <- polygons_sf %>%
  dplyr::select(all_of(c(polygonid_column, classid_column))) %>%
  sf::st_drop_geometry()

# Plan multi-process
future::plan("multisession", gc = TRUE, workers = 15)

# Get model output
model_result_df <- readRDS(paste0(repo_source, "/", model_result))

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
  # Add Repetition Indiator
  rep_accuracy_data$RepetitionNumber <- repetition_list[[i]]
  # Add Data to Accuracy List
  map_accuracy_bags_list[[i]] <- rep_accuracy_data
}

# Bind Rows
map_accuracy_df <- map_accuracy_bags_list %>%
  dplyr::bind_rows() %>%
  dplyr::filter(ClassId == "C15") %>%
  dplyr::mutate(ClassName := "Tomato")

# Create the Plot
rep_plot <- ggplot(
  map_accuracy_df,
  aes(x=Accuracy, fill=ClassName)) +
  theme(legend.title = element_blank()) +
  geom_histogram(color="black", bins = 30) +
  scale_fill_manual(values = c("Tomato"="#FF0000")) +
  facet_wrap(. ~ RepetitionNumber)

# Save Plot
ggplot2::ggsave(
  filename = paste0(repo_source, "/visualizations",
                    "/bootmap_accuracyhistogram_differentreps_tomato.png"),
  width = 40, height = 30, units = "cm",
  dpi = "retina",
  plot = rep_plot)

# ALLUVIAL PLOT -----------------------------------------------------------

test_conf_df <- caret::confusionMatrix(
  data = as.factor(model_result_df[[prediction_column]]),
  reference = as.factor(model_result_df[[classid_column]]),
  positive = NULL,
  dnn = c("Prediction","Reference"))
test_conf_df <- test_conf_df$table %>% as.data.frame()

# Change ID Names
test_conf_df$Prediction %<>% as.character()
test_conf_df$Prediction <- paste0("C", test_conf_df$Prediction)
test_conf_df$Reference <- paste0("C", test_conf_df$Reference)
test_conf_df$Prediction %<>%dplyr::recode("C11" = "Winter Cultivation",
                                       "C12" = "Sunflower",
                                       "C13" = "Capia Pepper",
                                       "C14" = "Paddy",
                                       "C15" = "Tomato",
                                       "C16" = "Watermelon",
                                       "C17" = "Melon",
                                       "C18" = "Corn",
                                       "C19" = "Sugarbeet",
                                       "C21" = "Alfalfa",
                                       "C22" = "Orchard",
                                       "C23" = "Vegetation",
                                       "C24" = "Impervious Surface")
test_conf_df$Reference %<>% as.character()
test_conf_df$Reference %<>% dplyr::recode("C11" = "Winter Cultivation ",
                                      "C12" = "Sunflower ",
                                      "C13" = "Capia Pepper ",
                                      "C14" = "Paddy ",
                                      "C15" = "Tomato ",
                                      "C16" = "Watermelon ",
                                      "C17" = "Melon ",
                                      "C18" = "Corn ",
                                      "C19" = "Sugarbeet ",
                                      "C21" = "Alfalfa ",
                                      "C22" = "Orchard ",
                                      "C23" = "Vegetation ",
                                      "C24" = "Impervious Surface ")

# Group & Sum values
test_conf_df %<>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarise(value = sum(Freq)) %>%
  as.data.frame()

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(test_conf_df$Reference),
                           as.character(test_conf_df$Prediction)) %>%
                      unique())

# With networkD3, connection must be provided using Reference, not using real name like in the links dataframe.. So we need to reformat it.
test_conf_df$IDsource=match(test_conf_df$Reference, nodes$name)-1 
test_conf_df$IDtarget=match(test_conf_df$Prediction, nodes$name)-1
test_conf_df$group=as.factor(test_conf_df$Reference)

# prepare color scale
ColourScal ='d3.scaleOrdinal() .domain([

"Winter Cultivation ", "Sunflower ", "Capia Pepper ", "Paddy ",
"Tomato ", "Watermelon ", "Melon ", "Corn ",
"Sugarbeet ", "Alfalfa ", "Orchard ",
"Vegetation ", "Impervious Surface "

.range([

"#FFFFFF", "#0044FF", "#4BFF00", "#00FFF2",
"#FF0000", "#107717", "#B1BC00", "#F0FF00",
"#720E91", "#46FFA1", "#FFFFFF",
"#FFFFFF", "#FFFFFF"])'

# Make the Network
p <- networkD3::sankeyNetwork(Links = test_conf_df, Nodes = nodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "value", NodeID = "name",
                              sinksRight=TRUE, colourScale=ColourScal,
                              LinkGroup = "group",
                              nodeWidth=40, fontSize=16, nodePadding=5)

htmlwidgets::onRender(p, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Reference", "Prediction"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i]);
    })
  }
')

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
