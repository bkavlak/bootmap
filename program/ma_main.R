# Load libraries
library(maptools, quietly = TRUE, warn.conflicts = FALSE)
library(raster, quietly = TRUE, warn.conflicts = FALSE)
library(rgdal, quietly = TRUE, warn.conflicts = FALSE)
library(caret, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(sf, quietly = TRUE, warn.conflicts = FALSE)
library(h2o4gpu, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(gdalUtils, quietly = TRUE, warn.conflicts = FALSE)
library(magrittr, quietly = TRUE, warn.conflicts = FALSE)
options(warn=-1)
set.seed(1773)


# Define Program Root Directory
root <- "/home/ziya/Desktop/gitHub/doktaR"

# Load Functions
source(paste0(root, "/import_doktaR.R"))
import_doktaR(root)
source(paste0(root, "/programs/map-accuracy/ma_setup.R"))

# Setting up the program on a root
ma_setup_folder()

# EXTRACT TEST PIXELS -----------------------------------------------------

## Read Vector & Raster Data
map_brick <- raster::brick(paste0(map_source, "/", map_name))
vector_sf <- sf::read_sf(paste0(vector_source, "/", vector_name))

## Filter ROI and Test Split
vector_sf %<>%
  dplyr::filter(CityNameEng == 'KONYA' &
                !!dplyr::sym(test_split_col) == 'TestSet')

future::plan("multisession")

## Extract pixels from raster
pixel_list <- class_list %>%
  furrr::future_map(
    ~ extract_class_obj(
      class_num = .x,
      raster_name = map_brick,
      vector_name = vector_sf,
      vector_cols = c("CityNameEng", "DistrictNameEng",
                      "ClassNameP3", "ProcessIdP3",
                      "TrainTestSplitP3", 'FieldId'),
      att_name = response_col,
      nd = nd
  ))
pixel_df   <- dplyr::bind_rows(pixel_list)

## Omit NA
pixel_df %<>% na.omit()

## Create PixelId Colon
pixel_df$PixelId <- 1:nrow(pixel_df)

## Add Source Raster Name
pixel_df$SourceRaster <- map_name

## Write pixel values to csv
data.table::fwrite(pixel_df, paste0(pixel_source, "/", pixel_name), bom = TRUE)


# BOOTSTRAP TEST ----------------------------------------------------------
## Read Pixels
accuracy_df <- data.table::fread(paste0(pixel_source, "/", pixel_name))

## Drop geom from test data
test_df <- vector_sf %>%
  dplyr::select("FieldId", response_col) %>%
  sf::st_drop_geometry()

## Bagging Test Data
boot_bag_list <- furrr::future_map(
  1:1500,
  ~ boot_sample(
    test_df,
    nrow(test_df),
    polid_column = "FieldId",
    class_column = response_col))

### Save Indices
saveRDS(boot_bag_list,
        paste0(bootdata_source, "/", boot_indices_name))


## Calculate accuracy for each bag
boot_df_list <- boot_bag_list %>%
  furrr::future_map(
    ~ boot_accuracy(
      data = accuracy_df,
      boot_indices = .x,
      index_column = "FieldId",
      class_col = response_col,
      pred_col = "MapClass"))

### Save accuracy df list
saveRDS(boot_df_list,
        paste0(bootdata_source, "/", boot_df_name))

## Visualize Bootstrap Accuracy Table
acc_table <- boot_table(boot_df_list,
                        type = "Accuracy")

boot_table_vis(table_data = acc_table,
               plot_name = boot_acc_tn,
               plot_source = bootdata_source,
               type = "Accuracy", # or "Accuracy"
               class_list = c("C12", "C13", "C15", "C17",
                              "C18", "C19", "C21", "C28",
                              "C32", "C41", "C43", "C52",
                              "C62", "C96", "C97", "C98",
                              "C99"),
               class_decoder = c("Sunflower", "Pepper", "Tomato", "Melon",
                                 "Corn", "Sugarbeet", "Alfalfa", "Potato",
                                 "Vegetation", "Wheat", "Barley", "Onion",
                                 "Squash", "Orchard", "NonField", "Other",
                                 "AgriculturalSoil"))

## Visualize Bootstrap Accuracy Histogram
vis_data <- boot_df_list %>% prepare_accuracy_vis()

# Generate Random Color
randomcoloR::randomColor(count = length(class_list))

boot_vis(vis_data = vis_data,
         plot_name = boot_acc_pn,
         plot_source = plot_source,
         type = "Accuracy", # or "Area"
         class_list = c("C12", "C13", "C15", "C17",
                        "C18", "C19", "C21", "C28",
                        "C32", "C41", "C43", "C52",
                        "C62", "C96", "C97", "C98",
                        "C99"),
         class_decoder = c("Sunflower", "Pepper", "Tomato", "Melon",
                           "Corn", "Sugarbeet", "Alfalfa", "Potato",
                           "Vegetation", "Wheat", "Barley", "Onion",
                           "Squash", "Orchard", "NonField", "Other",
                           "AgriculturalSoil"),
         fill_cols = c("Sunflower"="#fce2bf",
                       "Pepper"="#70e868",
                       "Tomato"="#6ffc94",
                       "Melon"="#82f2ea",
                       "Corn"="#dd237d",
                       "Sugarbeet"="#5db5dd",
                       "Alfalfa"="#aedb46",
                       "Potato"="#6df27c",
                       "Vegetation"="#136e7c",
                       "Wheat"="#7cefe0",
                       "Barley"="#ff9a42",
                       "Onion"="#5c6be0",
                       "Squash"="#d34a6c",
                       "Orchard"="#bf1c16",
                       "NonField"="#80ed6a",
                       "Other"="#8469bf",
                       "AgriculturalSoil"="#396d02"))

# BOOTSTRAP AREA ADJUSTMENT -----------------------------------------------

## Adjust Area for each bag
boot_area_list <- boot_df_list %>%
  furrr::future_map(
    ~ adjust_area(conf_matrix = .x,
                  area_matrix = data.frame(area_list,
                                           row.names = class_list)))

## Visualize Bootstrap Area Table
area_table <- boot_table(boot_area_list,
                         type = "Area")
area_table$AreaEstimation <- area_list

boot_table_vis(table_data = area_table,
               plot_name = boot_area_tn,
               plot_source = bootdata_source,
               type = "Area", # or "Accuracy"
               class_list = c("C12", "C13", "C15", "C17",
                              "C18", "C19", "C21", "C28",
                              "C32", "C41", "C43", "C52",
                              "C62", "C96", "C97", "C98",
                              "C99"),
               class_decoder = c("Sunflower", "Pepper", "Tomato", "Melon",
                                 "Corn", "Sugarbeet", "Alfalfa", "Potato",
                                 "Vegetation", "Wheat", "Barley", "Onion",
                                 "Squash", "Orchard", "NonField", "Other",
                                 "AgriculturalSoil"))

## Visualize Bootstrap Area Histogram
vis_data <- boot_area_list %>% prepare_area_vis()

boot_vis(vis_data = vis_data,
         plot_name = boot_area_pn,
         plot_source = plot_source,
         type = "Area", # or "Accuracy"
         class_list = c("C12", "C13", "C15", "C17",
                        "C18", "C19", "C21", "C28",
                        "C32", "C41", "C43", "C52",
                        "C62", "C96", "C97", "C98",
                        "C99"),
         class_decoder = c("Sunflower", "Pepper", "Tomato", "Melon",
                           "Corn", "Sugarbeet", "Alfalfa", "Potato",
                           "Vegetation", "Wheat", "Barley", "Onion",
                           "Squash", "Orchard", "NonField", "Other",
                           "AgriculturalSoil"),
         fill_cols = c("Sunflower"="#fce2bf",
                       "Pepper"="#70e868",
                       "Tomato"="#6ffc94",
                       "Melon"="#82f2ea",
                       "Corn"="#dd237d",
                       "Sugarbeet"="#5db5dd",
                       "Alfalfa"="#aedb46",
                       "Potato"="#6df27c",
                       "Vegetation"="#136e7c",
                       "Wheat"="#7cefe0",
                       "Barley"="#ff9a42",
                       "Onion"="#5c6be0",
                       "Squash"="#d34a6c",
                       "Orchard"="#bf1c16",
                       "NonField"="#80ed6a",
                       "Other"="#8469bf",
                       "AgriculturalSoil"="#396d02"))

# ALLUVIAL PLOT -----------------------------------------------------------

conf_mat <- caret::confusionMatrix(as.factor(accuracy_df$MapClass),
                                   as.factor(accuracy_df$ProcessIdP3),
                                   positive = NULL, dnn = c("Prediction","Reference"))
conf_mat <- conf_mat$table %>% as.data.frame()

# Change ID Names
conf_mat$Prediction %<>% as.character()
conf_mat$Prediction <- paste0("C", conf_mat$Prediction)
conf_mat$Reference <- paste0("C", conf_mat$Reference)
conf_mat$Prediction %<>% dplyr::recode("C12" = "Sunflower",
                                       "C13" = "Pepper",
                                       "C15" = "Tomato",
                                       "C17" = "Melon",
                                       "C18" = "Corn",
                                       "C19" = "Sugarbeet",
                                       "C21" = "Alfalfa",
                                       "C28" = "Potato",
                                       "C32" = "Vegetation",
                                       "C41" = "Wheat",
                                       "C43" = "Barley",
                                       "C52" = "Onion",
                                       "C62" = "Squash",
                                       "C96" = "Orchard",
                                       "C97" = "NonField",
                                       "C98" = "Other",
                                       "C99" = "AgriculturalSoil")
conf_mat$Reference %<>% as.character()
conf_mat$Reference %<>% dplyr::recode("C12" = "Sunflower ",
                                      "C13" = "Pepper ",
                                      "C15" = "Tomato ",
                                      "C17" = "Melon ",
                                      "C18" = "Corn ",
                                      "C19" = "Sugarbeet ",
                                      "C21" = "Alfalfa ",
                                      "C28" = "Potato ",
                                      "C32" = "Vegetation ",
                                      "C41" = "Wheat ",
                                      "C43" = "Barley ",
                                      "C52" = "Onion ",
                                      "C62" = "Squash ",
                                      "C96" = "Orchard ",
                                      "C97" = "NonField ",
                                      "C98" = "Other ",
                                      "C99" = "AgriculturalSoil ")

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

# Generate Random Color
randomcoloR::randomColor(count = length(class_list))

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .domain([

"Sunflower", "Pepper", "Tomato", "Melon",
"Corn", "Sugarbeet", "Alfalfa", "Potato",
"Vegetation", "Wheat", "Barley", "Onion",
"Squash", "Orchard", "NonField", "Other",
"AgriculturalSoil",
                                 
"Sunflower ", "Pepper ", "Tomato ", "Melon ",
"Corn ", "Sugarbeet ", "Alfalfa ", "Potato ",
"Vegetation ", "Wheat ", "Barley ", "Onion ",
"Squash ", "Orchard ", "NonField ", "Other ",
"AgriculturalSoil "])

.range([

"#fce2bf", "#70e868", "#6ffc94", "#82f2ea",
"#dd237d", "#5db5dd", "#aedb46", "#6df27c",
"#136e7c", "#7cefe0", "#ff9a42", "#5c6be0",
"#d34a6c", "#bf1c16", "#80ed6a", "#8469bf",
"#396d02",

"#fce2bf", "#70e868", "#6ffc94", "#82f2ea",
"#dd237d", "#5db5dd", "#aedb46", "#6df27c",
"#136e7c", "#7cefe0", "#ff9a42", "#5c6be0",
"#d34a6c", "#bf1c16", "#80ed6a", "#8469bf",
"#396d02"])'

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
