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
#' @param congregate Only applies to Zika data. Since Zika data was tracked on a scattered weekly basis,
#' this determines whether or not the user would like to congregate the data (TRUE) or just to
#' see the data tracked on the selected date in the passed-in data frame (FALSE). Note that if congregate is
#' TRUE, the exact date won't be returned, but rather the closest congregation date prior to the selected date.
#' Defaults to FALSE. 
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
#' covid_data <- importCovidData()
#' sars_data <- importSARSData()
#' zika_data <- filterDiseaseData(importZikaData(), include_suspected = FALSE)
#' 
#' mapPlotStatic(covid_data, selected_date = "2020-03-19", 
#' selected_value_type = "cases", alpha = 0.3)
#' mapPlotStatic(sars_data, selected_date = "2003-04-02", 
#' selected_value_type = "deaths", color = "purple")
#' mapPlotStatic(zika_data, selected_date = "2016-05-03", 
#' selected_value_type = "cases", color = "purple", congregate = TRUE)
#' 
#' @export
#'

mapPlotStatic <- function(data, selected_date = NA, selected_value_type = NA, color = 'red', alpha = 0.5, congregate = FALSE) {
  # Ensures longitude column is present in data
  if(!("long" %in% colnames(data)) | !is.numeric(data$long)) {
    stop('\"long\" column is either missing from data or is not of type \"numeric\".')
  }
  # Ensures latitude column is present in data
  if(!("lat" %in% colnames(data)) | !is.numeric(data$lat)) {
    stop('\"lat\" column is either missing from data or is not of type \"numeric\".')
  }
  # Ensures value column is present in data
  if(!("value" %in% colnames(data)) | !is.numeric(data$value)) {
    stop('\"value\" column is either missing from data or is not of type \"numeric\".')
  }
  # Ensures multiple dates will not be plotted on top of one another
  if("date" %in% colnames(data) & length(unique(data$date)) > 1) {
    if(is.na(selected_date)) {
      stop('Data contains multiple dates: please select one date as \"selected_date\" argument to be plotted.')
    } else {
      # Congregates data if requested - this can only be done with Zika data
      if(congregate) {
        data = congregateDataDates(data)
        dts = unique(data$date)
        
        plotted_date = as.Date(selected_date)
        while(!(plotted_date %in% dts)){
          plotted_date = plotted_date - 1
        }
        
        selected_date = plotted_date
        
      }
      data = data %>%
        dplyr::filter(date == as.Date(selected_date))
    }
  # Fills in date of data if available
  } else if(is.na(selected_date) & "date" %in% colnames(data) & 
            length(unique(data$date)) == 1) {
    selected_date = as.Date(unique(data$date[1]))
  }
  # Ensures one proper value type has been provided 
  if("value_type" %in% colnames(data) & length(unique(data$value_type)) > 1) {
    if(is.na(selected_value_type)) {
      stop('Data contains multiple types of value: please select one value type as \"selected_value_type\" argument to be plotted.')
    } else if(!(selected_value_type %in% c("cases", "deaths", "recovered"))) {
      stop('\"selected_value_type\" argument must be \"cases\", \"deaths\", or \"recovered\".')
    } else if("disease" %in% colnames(data) & unique(data$disease)[1] == "zika" & 
              selected_value_type != "cases") {
      stop('Zika data only contains data for \"cases\".')
    } else {
      data = data %>%
        dplyr::filter(value_type == selected_value_type)
    }
  # Fills in value type of data if available
  } else if(is.na(selected_value_type) & "value_type" %in% colnames(data) & 
            length(unique(data$value_type)) == 1) {
    selected_value_type = unique(data$value_type[1])
  }
  
  # Filters data
  data = data %>% 
    dplyr::group_by(date, lat, long) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::filter(value > 0)
  
  # Creates world map background
  world <- ggplot2::ggplot() +
    ggplot2::borders("world", colour = "gray85", fill = "gray80") +
    ggthemes::theme_map()
  
  # Generates plot
  map <- world + 
    ggplot2::geom_point(ggplot2::aes(x = long, y = lat, size = log10(value)),
               data = data, 
               colour = color, alpha = alpha) +
    ggplot2::labs(title = paste0("Number of ", 
                                 selected_value_type, 
                                 " on ", 
                                 selected_date)) + 
    ggplot2::scale_size_continuous(name = paste0("Log10 of ", selected_value_type))
  
  return(map)
}

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
#' @importFrom gganimate transition_time animate
#' 
#' @examples
#' covid_data <- importCovidData()
#' sars_data <- importSARSData()
#' zika_data <- filterDiseaseData(importZikaData(), include_suspected = FALSE)
#' mapPlotAnimate(covid_data, dps = 3)
#' mapPlotAnimate(sars_data, selected_value_type = "recovered")
#' mapPlotAnimate(zika_data, color = "green", alpha = 0.7)
#' 
#' @export
#'
mapPlotAnimate <- function(data, first_date = NA, last_date = NA, selected_value_type = NA, color = 'red', alpha = 0.5, dps = 5) {
  # Ensures longitude column is present in data
  if(!("long" %in% colnames(data)) | !is.numeric(data$long)) {
    stop('\"long\" column is either missing from data or is not of type \"numeric\".')
  }
  # Ensures latitude column is present in data
  if(!("lat" %in% colnames(data)) | !is.numeric(data$lat)) {
    stop('\"lat\" column is either missing from data or is not of type \"numeric\".')
  }
  # Ensures value column is present in data
  if(!("value" %in% colnames(data)) | !is.numeric(data$value)) {
    stop('\"value\" column is either missing from data or is not of type \"numeric\".')
  }
  # Ensures date values provided are of proper form
  if(is.na(first_date) & "date" %in% colnames(data)) {
    if(is.na(first_date)) {
      first_date = min(data$date)
    }
    if(is.na(last_date) & "date" %in% colnames(data)) {
      last_date = max(data$date)
    }
  }
  # Ensures one proper value type has been provided
  if("value_type" %in% colnames(data) & length(unique(data$value_type)) > 1) {
    if(is.na(selected_value_type)) {
      selected_value_type = "cases"
    } else if(!(selected_value_type %in% c("cases", "deaths", "recovered"))) {
      stop('\"selected_value_type\" argument must be \"cases\", \"deaths\", or \"recovered\".')
    } else if("disease" %in% colnames(data) & unique(data$disease)[1] == "zika" & 
              selected_value_type != "cases") {
      stop('Zika data only contains data for \"cases\".')
    }
  }
  
  # Congregates Zika data
  if(unique(data$disease)[1] == "zika") {
    data = congregateDataDates(data)
  }
  
  # Eliminates NA rows found in SARS data
  if(unique(data$disease)[1] == "sars") {
    data = data %>%
      dplyr::filter(!is.na(region))
  }
  
  # Filters data
  data = data %>% 
    dplyr::filter(date >= as.Date(first_date) & date <= as.Date(last_date)) %>%
    dplyr::filter(value_type == selected_value_type) %>%
    dplyr::group_by(date, lat, long) %>%
    dplyr::summarise(value = sum(value))
  
  # Generates world map background
  world <- ggplot2::ggplot() +
    ggplot2::borders("world", colour = "gray85", fill = "gray80") +
    ggthemes::theme_map()
  
  # Generates plots
  map <- world + 
    ggplot2::geom_point(ggplot2::aes(x = long, y = lat, size = log10(value)),
                        data = data, 
                        colour = color, alpha = alpha) +
    gganimate::transition_time(date) +
    ggplot2::labs(title = "Date: {frame_time}") + 
    ggplot2::scale_size_continuous(name = paste0("Number of ", selected_value_type),
                          range = c(1,6), breaks = c(0,1,2,3,4,5), 
                          labels = c(1,10,100,1000,10000,100000))
  
  #Animates plots at desired speed
  gganimate::animate(map, fps = (dps*2))
}

