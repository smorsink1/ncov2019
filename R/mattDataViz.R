#' Map Plot
#' 
#' Generates a global map plot of a certain value of data.
#' 
#' @param data The data frame that contains the data to be plotted on the world map.
#' This data frame must contain the following columns: "long" (type "numeric"), 
#' "lat" (type "numeric"), and "value" (type "numeric").
#' @param selected_date If the data frame contains a column "date" with multiple dates,
#' this parameter must specify a specific date to be plotted.
#' @param selected_value_type If the data frame contains a column "value_type" with multiple dates,
#' this parameter must specify a specific data value type to be plotted. Options are "cases", "deaths", 
#' and "recovered".
#' @param color Color of points on world map. Default is red.
#' @param alpha Transparency level of points on world map. Default is 0.5.
#' 
#' @return Output is the ggplot object which maps the chosen value on a world map using
#' latitude and longitude data. 
#' 
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot borders geom_point
#' @importFrom ggthemes theme_map
#' 
#' @examples 
#' map_plot(importCovidData(), selected_date = "2020-02-19", selected_value_type = "cases", alpha = 0.3)
#' map_plot(importSARSData(), selected_date = "2003-04-02", selected_value_type = "deaths", color = "purple"))
#' 
#' @export
#'

map_plot <- function(data, selected_date = NA, selected_value_type = NA, color = 'red', alpha = 0.5) {
  if(!("long" %in% colnames(data)) | !is.numeric(data$long)) {
    stop('\"long\" column is either missing from data or is not of type \"numeric\".')
  }
  if(!("lat" %in% colnames(data)) | !is.numeric(data$lat)) {
    stop('\"lat\" column is either missing from data or is not of type \"numeric\".')
  }
  if(!("value" %in% colnames(data)) | !is.numeric(data$value)) {
    stop('\"value\" column is either missing from data or is not of type \"numeric\".')
  }
  if("date" %in% colnames(data) & length(unique(data$date)) > 1) {
    if(is.na(selected_date)) {
      stop('Data contains multiple dates: please select one date as \"selected_date\" argument to be plotted.')
    } else {
      data = data %>%
        dplyr::filter(date == as.Date(selected_date))
    }
  }
  if("value_type" %in% colnames(data) & length(unique(data$value_type)) > 1) {
    if(is.na(selected_value_type)) {
      stop('Data contains multiple types of value: please select one value type as \"selected_value_type\" argument to be plotted.')
    } else {
      data = data %>%
        dplyr::filter(value_type == selected_value_type)
    }
  }
  
  data = data %>% filter(value > 0)
  
  world <- ggplot2::ggplot() +
    ggplot2::borders("world", colour = "gray85", fill = "gray80") +
    ggthemes::theme_map()
  
  map <- world + 
    ggplot2::geom_point(aes(x = long, y = lat, size = value),
               data = data, 
               colour = color, alpha = alpha)
  
  return(map)
}