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


# Define Program Root Directory
root <- ".../github/bootmap"

# Source Functions
source(paste0(root, "/functions/boot_sample.R"))
source(paste0(root, "/functions/boot_stat.R"))
source(paste0(root, "/functions/boot_accuracy.R"))
source(paste0(root, "/functions/boot_table.R"))
source(paste0(root, "/functions/boot_table_vis.R"))
source(paste0(root, "/functions/boot_vis.R"))
source(paste0(root, "/functions/adjust_area.R"))
source(paste0(root, "/functions/calc_interval_frame.R"))
source(paste0(root, "/functions/calc_prod_accuracy.R"))
source(paste0(root, "/functions/calc_prod_matrix.R"))
source(paste0(root, "/functions/calc_user_matrix.R"))
source(paste0(root, "/functions/calc_user_accuracy.R"))
source(paste0(root, "/functions/prepare_accuracy_vis.R"))
source(paste0(root, "/functions/prepare_accuracy_table.R"))
source(paste0(root, "/functions/prepare_area_vis.R"))
source(paste0(root, "/functions/prepare_area_table.R"))
source(paste0(root, "/functions/drop_non_square.R"))
source(paste0(root, "/iterators/exp_boot_iterator.R"))
source(paste0(root, "/programs/boot_setup.R"))

# Setup Folders
boot_setup_folder(root)

# Get data
polygons   <- sf::st_read(paste0(polygon_source, "/", "FullSet_CV_v02.shp"))
test_polid <- polygons %>%
  subset(layer == "TestSet") %>%
  dplyr::select("polygonID", "id")
sf::st_geometry(test_polid) <- NULL

# Plan multi-process
future::plan("multisession", gc = TRUE, workers = 15)


# Set Polygon Id
polygon_id <- "polygonID"

# Get model outputs
model_list <- purrr::map(list.files(model_source, "*SingleTrainTest.rds"),
                         ~ readRDS(paste0(model_source, "/", .x)))

# PRODUCE INDICES ---------------------------------------------------------

# Bootstrap polygons
boot_list_50   <- furrr::future_map(1:50, ~ boot_sample(test_polid, nrow(test_polid), polid_column = "polygonID", class_column = "id"))
boot_list_100  <- furrr::future_map(1:100, ~ boot_sample(test_polid, nrow(test_polid), polid_column = "polygonID", class_column = "id"))
boot_list_200  <- furrr::future_map(1:200, ~ boot_sample(test_polid, nrow(test_polid), polid_column = "polygonID", class_column = "id"))
boot_list_500  <- furrr::future_map(1:500, ~ boot_sample(test_polid, nrow(test_polid), polid_column = "polygonID", class_column = "id"))
boot_list_1000 <- furrr::future_map(1:1000, ~ boot_sample(test_polid, nrow(test_polid), polid_column = "polygonID", class_column = "id"))
boot_list_1500 <- furrr::future_map(1:1500, ~ boot_sample(test_polid, nrow(test_polid), polid_column = "polygonID", class_column = "id"))

# Save as object
saveRDS(boot_list_50, paste0(indices_source, "polygon_bootstrap_indices_50_v05.rds"))
saveRDS(boot_list_100, paste0(indices_source, "polygon_bootstrap_indices_100_v05.rds"))
saveRDS(boot_list_200, paste0(indices_source, "polygon_bootstrap_indices_200_v05.rds"))
saveRDS(boot_list_500, paste0(indices_source, "polygon_bootstrap_indices_500_v05.rds"))
saveRDS(boot_list_1000, paste0(indices_source, "polygon_bootstrap_indices_1000_v05.rds"))
saveRDS(boot_list_1500, paste0(indices_source, "polygon_bootstrap_indices_1500_v05.rds"))


# PRODUCE ACCURACY LISTS --------------------------------------------------

# Polygon-based Bootstrap Accuracy Results
poly_boot_accuracy  <- vector("list", length(model_list))

# Simulate Confusion Matrix List
for (n in c(50, 100 , 200, 500, 1000, 1500)){
  
  # Read Boot Indices with Target Number
  polygon_boot_list <- readRDS(paste0(indices_source, "/", "polygon_bootstrap_indices_", n, "_v05.rds"))
  
  cat(paste0("Bootstrap with ", n, " number is started."), "\n")
  
  for (i in seq_along(model_list)) {
    
    cat(paste0("Bootstrap ", i, " is started."), "\n")
    
    # Add accuracy to list
    poly_boot_accuracy[[i]] <- furrr::future_map(polygon_boot_list, ~ boot_accuracy(model_list[[i]],
                                                                    .x,
                                                                    polygon_id))
    
    cat(paste0("Bootstrap ", i, " is finished."), "\n")
  }
  
  cat(paste0("Bootstrap ", n, " number is finished."), "\n")
  
  saveRDS(poly_boot_accuracy, paste0(accuracy_dest, "/", "polygon_bootstrap_accuracy_", n, "_v05.rds"))
}


# AREA ADJUSTMENT LIST ---------------------------------------------------

# Calculate Simulated Adjusted Area
for (n in c(50, 100 , 200, 500, 1000, 1500)){
  
  # Read Boot Indices with Target Number
  poly_boot_accuracy <- readRDS(paste0(accuracy_dest, "/", "polygon_bootstrap_accuracy_", n, "_v05.rds"))
  
  cat(paste0("Bootstrap with ", n, " number is started."), "\n")

  for (i in seq_along(model_list)) {
    
    cat(paste0("Bootstrap ", i, " is started."), "\n")
    
    # Add Adjusted Accuracy to list
    poly_boot_accuracy[[i]] %<>% furrr::future_map(~ adjust_area(.x, data.frame(c(2471.446, 539.2134, 890.6652,
                                                                                  2396.22, 5463.848, 504.6588,
                                                                                  97.4421, 6666.021, 661.9248,
                                                                                  322.569, 506.1456, 2335.543,
                                                                                  117.3798),
                                                                                row.names = c(11, 12, 13, 14,
                                                                                              15, 16, 17, 18,
                                                                                              19, 21, 22, 23, 24))))
    
    cat(paste0("Bootstrap ", i, " is finished."), "\n")
  }

  cat(paste0("Bootstrap ", n, " number is finished."), "\n")
  
  saveRDS(poly_boot_accuracy, paste0(accuracy_dest, "/", "polygon_bootstrap_area_", n, "_v05.rds"))
}


# BOOTSTRAP HISTOGRAM -----------------------------------------------------

# Get Bootstrap Indices
boot_list_names <- c("polygon_bootstrap_area_50_v05.rds",
                     "polygon_bootstrap_area_100_v05.rds",
                     "polygon_bootstrap_area_200_v05.rds",
                     "polygon_bootstrap_area_500_v05.rds",
                     "polygon_bootstrap_area_1000_v05.rds",
                     "polygon_bootstrap_area_1500_v05.rds")

# Experiment List
exp_list <- 27:27

for(list_name in boot_list_names){
  
  furrr::future_map(exp_list, ~ exp_boot_iterator(boot_list_name = list_name,
                                                  boot_list_source = accuracy_dest,
                                                  exp_number = .x,
                                                  plot_source = boot_histo_dest,
                                                  boot_str_order = 4,
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
                                                                "Road" = "#917d7d")))
}

# Get Bootstrap Indices
boot_list_names <- c("polygon_bootstrap_accuracy_50_v05.rds",
                     "polygon_bootstrap_accuracy_100_v05.rds",
                     "polygon_bootstrap_accuracy_200_v05.rds",
                     "polygon_bootstrap_accuracy_500_v05.rds",
                     "polygon_bootstrap_accuracy_1000_v05.rds",
                     "polygon_bootstrap_accuracy_1500_v05.rds")

# Experiment List
exp_list <- 27:27

for(list_name in boot_list_names){
  
  furrr::future_map(exp_list, ~ exp_boot_iterator(boot_list_name = list_name,
                                                  boot_list_source = accuracy_dest,
                                                  exp_number = .x,
                                                  plot_source = boot_histo_dest,
                                                  boot_str_order = 4,
                                                  type = "Accuracy", # or "Accuracy"
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
                                                                "Road" = "#917d7d")))
}

library(gifski)
png_files <- list.files(boot_histo_dest, pattern = ".*png$", full.names = TRUE)
gifski::gifski(png_files, gif_file = "animation.gif", width = 800, height = 600, delay = 1)

# ALLUVIAL PLOT -----------------------------------------------------------

conf_mat <- model_list[[27]]

conf_mat <- caret::confusionMatrix(as.factor(conf_mat$predictions),
                                   as.factor(conf_mat$id),
                                   positive = NULL, dnn = c("Prediction","Reference"))
conf_mat <- conf_mat$table %>% as.data.frame()

# Change ID Names
conf_mat$Prediction %<>% as.character()
conf_mat$Prediction <- paste0("C", conf_mat$Prediction)
conf_mat$Reference <- paste0("C", conf_mat$Reference)
conf_mat$Prediction %<>% dplyr::recode("C11" = "Soil",
                                        "C12" = "Sunflower",
                                        "C13" = "Pepper",
                                        "C14" = "Rice",
                                        "C15" = "Tomato",
                                        "C16" = "Watermelon",
                                        "C17" = "Melon",
                                        "C18" = "Corn",
                                        "C19" = "Sugarbeet",
                                        "C21" = "Alfalfa",
                                        "C22" = "Orchard",
                                        "C23" = "Vegetation",
                                        "C24" = "Road")
conf_mat$Reference %<>% as.character()
conf_mat$Reference %<>% dplyr::recode("C11" = "Soil ",
                               "C12" = "Sunflower ",
                               "C13" = "Pepper ",
                               "C14" = "Rice ",
                               "C15" = "Tomato ",
                               "C16" = "Watermelon ",
                               "C17" = "Melon ",
                               "C18" = "Corn ",
                               "C19" = "Sugarbeet ",
                               "C21" = "Alfalfa ",
                               "C22" = "Orchard ",
                               "C23" = "Vegetation ",
                               "C24" = "Road ")

# Group & Sum values
conf_mat %<>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarise(value = sum(Freq)) %>%
  as.data.frame()

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(conf_mat$Reference), as.character(conf_mat$Prediction)) %>% unique())

# With networkD3, connection must be provided using Reference, not using real name like in the links dataframe.. So we need to reformat it.
conf_mat$IDsource=match(conf_mat$Reference, nodes$name)-1 
conf_mat$IDtarget=match(conf_mat$Prediction, nodes$name)-1
conf_mat$group=as.factor(conf_mat$Reference)

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .domain(["Soil ", "Sunflower ", "Pepper ", "Rice ",
"Tomato ", "Watermelon ", "Melon ", "Corn ",
"Sugarbeet ", "Alfalfa ", "Orchard ",
"Vegetation ", "Road ",
"Soil", "Sunflower", "Pepper", "Rice",
"Tomato", "Watermelon", "Melon", "Corn",
"Sugarbeet", "Alfalfa", "Orchard",
"Vegetation", "Road"])
.range(["#fb8807", "#094aaa", "#f232ee", "#31a1c4", "#e70d46", "#21f429",
"#f8b70c", "#f8e00d", "#b460c5", "#1dd4ab", "#000000", "#122c0d", "#917d7d",
"#fb8807", "#094aaa", "#f232ee", "#31a1c4", "#e70d46", "#21f429",
"#f8b70c", "#f8e00d", "#b460c5", "#1dd4ab", "#000000", "#122c0d", "#917d7d"])'

# Make the Network
p <- networkD3::sankeyNetwork(Links = conf_mat, Nodes = nodes,
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


# BOOTSTRAP TABLE ---------------------------------------------------------

# Accuracy Table
conf_matrix_list <- readRDS(paste0(accuracy_dest, "/", "polygon_bootstrap_accuracy_1500_v05.rds"))
conf_matrix_list <- conf_matrix_list[[27]]
acc_table <- boot_table(conf_matrix_list,
                        type = "Accuracy")
boot_table_vis(acc_table = acc_table,
               plot_name = "Experiment45_BootTable_Accuracy_1500.png",
               plot_source = boot_table_dest,
               type = "Accuracy", # or "Accuracy"
               class_list = c("C11", "C12", "C13", "C14",
                        "C15", "C16", "C17", "C18",
                        "C19", "C21", "C22", "C23", "C24"),
               class_decoder = c("Soil", "Sunflower", "Pepper", "Rice",
                           "Tomato", "Watermelon", "Melon", "Corn",
                           "Sugarbeet", "Alfalfa", "Orchard",
                           "Vegetation", "Road"))

# Area Table
conf_matrix_list <- readRDS(paste0(accuracy_dest, "/", "polygon_bootstrap_area_1500_v05.rds")) %>%
  purrr::map(~ as.data.frame(t(as.data.frame(.x))))
conf_matrix_list <- conf_matrix_list[[27]]
acc_table <- boot_table(conf_matrix_list,
                        type = "Area")
# Add First Area Estimation Column
acc_table$AreaEstimation <- c(2471.446, 539.2134, 890.6652,
                              2396.22, 5463.848, 504.6588,
                              97.4421, 6666.021, 661.9248,
                              322.569, 506.1456, 2335.543,
                              117.3798)

boot_table_vis(acc_table = acc_table,
               plot_name = "Experiment45_BootTable_Area_1500.png",
               plot_source = boot_table_dest,
               type = "Area", # or "Accuracy"
               class_list = c("C11", "C12", "C13", "C14",
                              "C15", "C16", "C17", "C18",
                              "C19", "C21", "C22", "C23", "C24"),
               class_decoder = c("Soil", "Sunflower", "Pepper", "Rice",
                                 "Tomato", "Watermelon", "Melon", "Corn",
                                 "Sugarbeet", "Alfalfa", "Orchard",
                                 "Vegetation", "Road"))
