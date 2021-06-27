calc_interval_frame <- function(prepared_frame,
                                confidence = 0.95,
                                type = "Area") {
  
  # Calculate Probs Acc. to Confidence
  f_i <- (1 - confidence) / 2
  s_i <- 1 - f_i
  
  # Group Values
  interval_frame <- prepared_frame %>%
    dplyr::group_by(ClassId) %>%
    dplyr::summarise(FirstInterval = quantile(!!dplyr::sym(type), probs = f_i),
                     Mean = mean(!!dplyr::sym(type)),
                     Median = median(!!dplyr::sym(type)),
                     SecondInterval = quantile(!!dplyr::sym(type), probs = s_i))
  
  return(interval_frame)
  
}