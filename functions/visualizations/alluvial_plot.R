require(dplyr)
require(networkD3)
require(varhandle)

alluvial_plot <- function(
  test_df,
  prediction_column,
  reference_column,
  class_id = c(11, 12, 13, 14,
               15, 16, 17, 18,
               19, 21, 22, 23,
               24),
  class_decoder = c("Winter Cultivation", "Sunflower", "Capia Pepper", "Paddy",
                 "Tomato", "Watermelon", "Melon", "Corn",
                 "Orchard", "Alfalfa", "Impervious Surface", "Vegetation",
                 "Water"),
  class_color = c("#FFFFFF", "#0044FF", "#4BFF00", "#00FFF2",
                  "#FF0000", "#107717", "#B1BC00", "#F0FF00",
                  "#720E91", "#46FFA1", "#FFFFFF", "#FFFFFF",
                  "#FFFFFF")){
  # Convert whitespace to undersocre
  class_decoder <- gsub(" ", "_", class_decoder)
  
  # Prepare Class DF
  class_df <- data.frame(class_id, class_decoder, class_color,
                         stringsAsFactors = FALSE)
  class_df$class_reference_name <- class_decoder
  class_df$class_prediction_name <- paste0(class_decoder, "_") # adding an undersocre
                                                               # here to differentiate.
  
  # Filter classes from test_df
  test_df %<>%
    dplyr::filter(!!dplyr::sym(reference_column) %in% class_id) %>%
    dplyr::filter(!!dplyr::sym(prediction_column) %in% class_id)
  
  # Create Confusion Matrix & Get Table
  conf_df <- caret::confusionMatrix(
    data = as.factor(test_df[[prediction_column]]),
    reference = as.factor(test_df[[reference_column]]),
    positive = NULL,
    dnn = c("Prediction","Reference"))
  conf_df <- conf_df$table %>% as.data.frame()
  
  # Unfactor factor fields
  conf_df$Prediction %<>% varhandle::unfactor()
  conf_df$Reference %<>% varhandle::unfactor()
  
  # Add Reference Class Names
  conf_df <- class_df %>%
    dplyr::select(class_id, class_reference_name) %>%
    dplyr::left_join(conf_df, by = c("class_id" = "Reference")) %>%
    dplyr::rename(ReferencenId = class_id,
                  ReferenceName = class_reference_name)
  # Add Prediction Class Names
  conf_df <- class_df %>%
    dplyr::select(class_id, class_prediction_name) %>%
    dplyr::left_join(conf_df, by = c("class_id" = "Prediction")) %>%
    dplyr::rename(PredictionId = class_id,
                  PredictionName = class_prediction_name)
  
  # Group & Sum values
  conf_df %<>%
    dplyr::group_by(PredictionName, ReferenceName) %>%
    dplyr::summarise(value = sum(Freq)) %>%
    as.data.frame()
  
  # From these flows we need to create a node data frame:
  # it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(conf_df$ReferenceName),
                             as.character(conf_df$PredictionName)) %>%
                        unique())
  nodes <- data.frame(name=c(class_decoder, paste0(class_decoder, "_")))
  nodes$group <- c(class_decoder, class_decoder)
  
  # With networkD3, connection must be provided using Reference,
  # not using real name like in the links dataframe.. So we need to reformat it.
  conf_df$IDsource=match(conf_df$ReferenceName, nodes$name)-1 
  conf_df$IDtarget=match(conf_df$PredictionName, nodes$name)-1
  conf_df$group=factor(conf_df$ReferenceName)
  
  # Prepare Class Names as String
  reference_name_str <- paste0('"', class_df$class_reference_name, '"')
  reference_name_str %<>% paste0(collapse = ',')
  prediction_name_str <- paste0('"', class_df$class_prediction_name, '"')
  prediction_name_str %<>% paste0(collapse = ',')
  # Prepare Class Colors as String
  class_color_str <- paste0('"', class_df$class_color, '"')
  class_color_str %<>% paste0(collapse = ',')
  
  # prepare color scale
  ColourScal <- paste0('d3.scaleOrdinal().domain([',
                       reference_name_str,
                       ']).range([',
                       class_color_str, '])')
  
  # Make the Network
  p <- networkD3::sankeyNetwork(
    Links = conf_df, Nodes = nodes,
    Source = "IDsource", Target = "IDtarget",
    Value = "value", NodeID = "name",
    sinksRight=TRUE, colourScale=ColourScal,
    LinkGroup = "group", NodeGroup = "group",
    nodeWidth=40, fontSize=0, nodePadding=5)
  
  return(p)
  
}