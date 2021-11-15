boot_table <- function(bag_list,
                       type = "Accuracy" # or "Area"
){
  
  if (type == "Accuracy"){
  
    # Prepare data
    acc_frame_user <- bag_list %>% prepare_accuracy_table(type = "User")
    acc_frame_prod <- bag_list %>% prepare_accuracy_table(type = "Producer")
    
    # Summarise table
    acc_frame_user %<>%
      dplyr::group_by(ClassId) %>%
      dplyr::summarise(
        UserFirstInterval  = round(quantile(Accuracy, probs = 0.025), 2),
        UserMeanAccuracy   = round(mean(Accuracy), 2),
        UserMedianAccuracy = round(quantile(Accuracy, probs = 0.5), 2),
        UserSecondInterval = round(quantile(Accuracy, probs = 0.975), 2))
    acc_frame_prod %<>%
      dplyr::group_by(ClassId) %>%
      dplyr::summarise(
        ProdFirstInterval  = round(quantile(Accuracy, probs = 0.025), 2),
        ProdMeanAccuracy   = round(mean(Accuracy), 2),
        ProdMedianAccuracy = round(quantile(Accuracy, probs = 0.5), 2),
        ProdSecondInterval = round(quantile(Accuracy, probs = 0.975), 2))
    
    # Left Join
    acc_table <- dplyr::left_join(acc_frame_user, acc_frame_prod)
    
    return(acc_table)
    
  } else if (type == "Area"){
    
    # Prepare data
    area_frame_user <- bag_list %>%
      prepare_area_table()
    
    # Summarise table
    area_frame_user %<>%
      dplyr::group_by(ClassId) %>%
      dplyr::summarise(
        UserFirstInterval  = round(quantile(Area, probs = 0.025), 2),
        UserMeanArea       = round(mean(Area), 2),
        UserMedianArea     = round(quantile(Area, probs = 0.5), 2),
        UserSecondInterval = round(quantile(Area, probs = 0.975), 2))
    
    return(area_frame_user)
    
  }
  
}