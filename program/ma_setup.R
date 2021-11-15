ma_setup_folder <- function() {
  
  # Define Vector Source & Name
  vector_source <<- "./Desktop/gitHub/common-files/Sample/Crop-Type/Turkey/2021/GPKG/"
  vector_name <<- "SAMPLE_TURKEY_2021_v40.gpkg"
  
  # Define Extracted Pixel Directory & Name
  pixel_source <<- "./Desktop/gitHub/doktaR/programs/map-accuracy/Data"
  pixel_name <<- "TESTPIXELS_KONYA_P3_2021_v01.csv"
  
  # Define Map Output Directory & name
  map_source <<- "./SATELLITE/RESULTS/CLASSIFICATION_RASTERS/2021"
  map_name <<- "GTS_KONYA_P3_2021_SUNFLOWERSUGARBEET_v01.tif"
  
  # Bootstrap Data
  bootdata_source <<- "./Desktop/gitHub/doktaR/programs/map-accuracy/Data"
  boot_indices_name <<- "BOOTINDICES_KONYA_P3_2021_v01.rds"
  boot_df_name <<- "BOOTDATAFRAME_KONYA_P3_2021_v01.rds"
  boot_acc_tn <<- "BOOTTABLEACCURACY_KONYA_P3_2021_v01.png"
  boot_area_tn <<- "BOOTTABLEAREA_KONYA_P3_2021_v01.png"
  
  # Define Plot Directory & Name
  plot_source <<- "./Desktop/gitHub/doktaR/programs/map-accuracy/Data"
  boot_acc_pn <<- "BOOTHISTACCURACY_KONYA_P3_2021_v01.png"
  boot_area_pn <<- "BOOTHISTAREA_KONYA_P3_2021_v01.png"
  
  # Program Variables
  test_split_col <<- "TrainTestSplitP3"
  response_col  <<- "ProcessIdP3"
  class_list    <<- c(12, 13, 15, 17,
                      18, 19, 21, 28,
                      32, 41, 43, 52,
                      62, 96, 97, 98,
                      99)
  area_list <<- c(1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000,
                  1000)
  nd <<- 0
  
}