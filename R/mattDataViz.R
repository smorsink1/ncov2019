#' Static Map Plot
#' 
#' Generates a static global map plot of a certain value of data.
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
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot borders geom_point aes labs scale_size_continuous
#' @importFrom ggthemes theme_map
#' 
#' @examples 
#' mapPlotStatic(importCovidData(), selected_date = "2020-02-19", selected_value_type = "cases", alpha = 0.3)
#' mapPlotStatic(importSARSData(), selected_date = "2003-04-02", selected_value_type = "deaths", color = "purple"))
#' 
#' @export
#'

mapPlotStatic <- function(data, selected_date = NA, selected_value_type = NA, color = 'red', alpha = 0.5) {
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
  } else if("date" %in% colnames(data) & length(unique(data$date)) == 1) {
    selected_date = as.Date(unique(data$date[1]))
  }
  if("value_type" %in% colnames(data) & length(unique(data$value_type)) > 1) {
    if(is.na(selected_value_type)) {
      stop('Data contains multiple types of value: please select one value type as \"selected_value_type\" argument to be plotted.')
    } else {
      data = data %>%
        dplyr::filter(value_type == selected_value_type)
    }
  } else if("value_type" %in% colnames(data) & length(unique(data$value_type)) == 1) {
    selected_value_type = unique(data$value_type[1])
  }
  
  data = data %>% 
    dplyr::group_by(date, lat, long) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::filter(value > 0)
  
  world <- ggplot2::ggplot() +
    ggplot2::borders("world", colour = "gray85", fill = "gray80") +
    ggthemes::theme_map()
  
  map <- world + 
    ggplot2::geom_point(ggplot2::aes(x = long, y = lat, size = log10(value)),
               data = data, 
               colour = color, alpha = alpha) +
    ggplot2::labs(title = paste0("Number of ", 
                                 selected_value_type, 
                                 " on ", 
                                 selected_date)) + 
    ggplot2::scale_size_continuous(name = paste0("Number of ", selected_value_type),
               range = c(1,6), breaks = c(0,1,2,3,4,5), 
               labels = c(1,10,100,1000,10000,100000))
  
  return(map)
}

mapPlotStatic(covid, selected_date = "2020-03-09", selected_value_type = "cases")

#' Animated Map Plot
#' 
#' Generates an animated global map plot of a certain value of data.
#' 
#' @param data The data frame that contains the data to be plotted on the world map.
#' This data frame must contain the following columns: "long" (type "numeric"), 
#' "lat" (type "numeric"), and "value" (type "numeric").
#' @param first_date This is the first date shown in the animation.
#' @param last_date This is the last date shown in the animation.
#' @param selected_value_type If the data frame contains a column "value_type" with multiple dates,
#' this parameter must specify a specific data value type to be plotted. Options are "cases", "deaths", 
#' and "recovered".
#' @param color Color of points on world map. Default is red.
#' @param alpha Transparency level of points on world map. Default is 0.5.
#' @param dps Number of dates displayed per second for the animation. Default is 5.
#' 
#' @return Output is an animated ggplot object which maps the chosen value on a world map using
#' latitude and longitude data over the range of dates specified. 
#' 
#' @importFrom dplyr filter group_by summarise
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot borders geom_point aes labs scale_size_continuous
#' @importFrom ggthemes theme_map
#' @importFrom gifski gifski
#' @imprtFrom gganimate transition_time animate
#' 
#' @examples 
#' mapPlotAnimate(importCovidData())
#' mapPlotAnimate(importSARSData())
#' 
#' @export
#'
mapPlotAnimate <- function(data, first_date = NA, last_date = NA, selected_value_type = NA, color = 'red', alpha = 0.5, dps = 5) {
  
  if(!("long" %in% colnames(data)) | !is.numeric(data$long)) {
    stop('\"long\" column is either missing from data or is not of type \"numeric\".')
  }
  if(!("lat" %in% colnames(data)) | !is.numeric(data$lat)) {
    stop('\"lat\" column is either missing from data or is not of type \"numeric\".')
  }
  if(!("value" %in% colnames(data)) | !is.numeric(data$value)) {
    stop('\"value\" column is either missing from data or is not of type \"numeric\".')
  }
  if(is.na(first_date) & "date" %in% colnames(data)) {
    if(is.na(first_date)) {
      first_date = min(data$date)
    }
    if(is.na(last_date) & "date" %in% colnames(data)) {
      last_date = max(data$date)
    }
  }
  if("value_type" %in% colnames(data) & length(unique(data$value_type)) > 1) {
    if(is.na(selected_value_type)) {
      selected_value_type = "cases"
    }
  }
  
  if(unique(data$disease)[1] == "zika") {
    data = congregateDataDates(data, frequency = "weekly")
  } else if(unique(data$disease)[1] == "sars") {
    data = congregateDataDates(data, frequency = "daily")
  }
  
  data = data %>% 
    dplyr::filter(date >= as.Date(first_date) & date <= as.Date(last_date)) %>%
    dplyr::filter(value_type == selected_value_type) %>%
    dplyr::group_by(date, lat, long) %>%
    dplyr::summarise(value = sum(value))
  
  world <- ggplot2::ggplot() +
    ggplot2::borders("world", colour = "gray85", fill = "gray80") +
    ggthemes::theme_map()
  
  map <- world + 
    ggplot2::geom_point(ggplot2::aes(x = long, y = lat, size = log10(value)),
                        data = data, 
                        colour = color, alpha = alpha) +
    gganimate::transition_time(date) +
    ggplot2::labs(title = "Date: {frame_time}") + 
    ggplot2::scale_size_continuous(name = paste0("Number of ", selected_value_type),
                          range = c(1,6), breaks = c(0,1,2,3,4,5), 
                          labels = c(1,10,100,1000,10000,100000))
  
  gganimate::animate(map, fps = (dps*2))
}

mapPlotAnimate(data = zika_confirmed, selected_value_type = "cases", color = "purple")

