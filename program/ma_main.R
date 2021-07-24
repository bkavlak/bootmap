# Load libraries ####
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

# Import Functions
source(paste0(root, "/programs/map-accuracy/ma_setup.R"))
source(paste0(root, "/functions/b_processing/import_recipe.R"))
source(paste0(root, "/functions/a_preprocessing/extract_class.R"))
source(paste0(root, "/functions/a_preprocessing/assign_fold.R"))
source(paste0(root, "/functions/_general/increase_version.R"))
source(paste0(root, "/functions/stat/boot_sample.R"))
source(paste0(root, "/functions/stat/boot_stat.R"))
source(paste0(root, "/functions/stat/boot_accuracy.R"))
source(paste0(root, "/functions/stat/boot_vis.R"))
source(paste0(root, "/functions/stat/adjust_area.R"))
source(paste0(root, "/functions/stat/calc_interval_frame.R"))
source(paste0(root, "/functions/stat/calc_prod_accuracy.R"))
source(paste0(root, "/functions/stat/calc_prod_matrix.R"))
source(paste0(root, "/functions/stat/calc_user_matrix.R"))
source(paste0(root, "/functions/stat/calc_user_accuracy.R"))
source(paste0(root, "/functions/stat/prepare_accuracy_vis.R"))
source(paste0(root, "/functions/stat/prepare_area_vis.R"))
source(paste0(root, "/functions/stat/drop_non_square.R"))

# Setting up the program on a root
ma_setup_folder(root = root)

# Set Extraction Arguments
args <- list(class_nums    = class_nums,
             raster_source = map_source,
             raster_name   = map_name,
             vector_source = vector_source,
             vector_name   = vector_name,
             att_name      = response_col,
             nd            = nd,
             filter_pol    = FALSE)

# Collect in a list and bind all the rows
cat("Create training data to train model\n")
future::plan("multisession")
list_train <- furrr::future_pmap(args, extract_class)
full_data <- dplyr::bind_rows(list_train)

# Omit NA
na_cols <- colnames(full_data) %>% tail(n = length(class_nums))
full_data <- na.omit(full_data, cols = na_cols)

# Create Pixel ID
full_data <- as.data.frame(append(full_data, list(seq.int(nrow(full_data))), after=(ncol(full_data) - length(band_list))))
colnames(full_data)[ncol(full_data) - length(band_list)] <- "PixelId"

# Change column names
colnames(full_data)[(ncol(full_data) - length(band_list) + 1):ncol(full_data)] <- band_list

# Write training samples to csv
data.table::fwrite(full_data, paste0(pixel_source, "/", pixel_name), bom = TRUE)

# READ TRAINING DATA
accuracy_data <- data.table::fread(paste0(pixel_source, "/", pixel_name))

