# ABBREVIATIONS
# RS  -> Raster Source
# RSC -> Raster Scope
# RD  -> Raster Destination
# RPD -> Report Destination
# UD  -> Update Destination
# VD  -> Vector Destination
# CD  -> CSV Destination
# KD  -> KML Destination
# RN  -> Raster Name
# ON  -> Output Name
# RL  -> Raster List
# VL  -> Vector List
# SN  -> Sort Number
# TS  -> TKGM Source
# HS  -> HGM Source
# TN  -> TKGM Name
# HN  -> HGM Name
  
ma_setup_folder <- function(root) {
  
  #' It sets up the program to root directory where all files are places.
  #' 
  #' @param root A string that defines the root directory

  # Set directory to root
  setwd(root)
  
  # Define Vector Source & Name
  vector_source <<- "/home/ziya/Desktop/PROCESS/TAT/2021/KUCUKMENDERES/SAMPLES"
  vector_name <<- "SampleCT_KUCUKMENDERES_2021_v03.gpkg"
  
  # Define Extracted Pixel Directory & Name
  pixel_source <<- "/home/ziya/Desktop/PROCESS/TAT/2021/KUCUKMENDERES/RAW_DATA"
  pixel_name <<- "S2_TAT_20210527_Pixel_v02.csv"
  
  # Define Map Output Directory & name
  map_source <<- "/home/ziya/Desktop/PROCESS/TAT/2021/KUCUKMENDERES/MAP"
  map_name <<- "MT_KUCUKMENDERES_P3_2021_TOMATO_v01.tif"
  
  # Define Plot Directory & Name
  plot_source <<- "/home/ziya/Desktop/PROCESS/TAT/2021/KUCUKMENDERES/RAW_DATA"
  plot_name <<- "GTS_CUKUROVA_P1_2021_DIVERSE_v01.png"
  
  # Program Variables
  response_col  <<- "ProcessId"
  class_number  <<- 9
  class_nums <<- c(13, 15, 18, 24, 32, 38, 41, 43, 98)
  nd <<- 0
  
}