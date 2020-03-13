

covid_data <- importCovidData()
us_covid_data <- filterDiseaseData(covid_data, country = "US")
china_covid_data <- filterDiseaseData(covid_data, country = "China")
japan_covid_data <- filterDiseaseData(covid_data, country = "Japan")

#' @import ggplot2
#' @importFrom dplyr group_by summarize filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
plotTimeSeries(data, plot_what) {
  # given a dataframe,
  # by default it plots from the start date to the last date in the data frame
  # plot_what: cases, recoveries, deaths, recovered/cases, dead/cases, new cases,
  #          : new cases/new cases yesterday, cases/pop, deaths/pop
  
  ## Check the plot_what, make sure that the data has the value_type values to support 
  ##   that plot
  ## If not, throw an error saying the problem
  
  ## if there are more than one provinces in the data, it groups to region level
  ## if there are more than one regions in the data, it groups to world level
  if (length(unique(data$province)) > 1 && length(unique(data$region)) > 1) {
    # world level
    data_grouped <- data %>%
      dplyr::group_by(date, value_type) %>%
      dplyr::summarize(pop_2018 = 7.5e9, value = sum(value))
    title <- "World"
  } else if (length(unique(data$province)) <= 1 && length(unique(data$region)) <= 1) {
    # province level
    data_grouped <- data %>%
      dplyr::group_by(date, region, province, value_type) %>%
      dplyr::summarize(value = sum(value))
    # inform that population stats are not available on a province-level
    title <- paste0(data_grouped$province[1], ", ", data_grouped$region[1])
  } else {
    # region level
    data_grouped <- data %>%
      dplyr::group_by(date, pop_2018, region, value_type) %>%
      dplyr::summarize(value = sum(value))
    title <- data_grouped$region[1]
  }
  
  # this is the default behavior (for no plot_what)
  ggplot(data = data_grouped) +
    geom_line(aes(x = date, y = value, col = value_type))
  
  if (plot_what %in% c("cases", "deaths", "recovered")) {
    p <- data_grouped %>%
      dplyr::filter(value_type == plot_what) %>%
      ggplot() +
        geom_line(aes(x = date, y = value))
    return (p)
  }
  
  # for plot_what = deaths / cases, recovered / cases, cases / pop_2018, etc.
  if (plot_what %in% c("deaths_per_cases", "recovered_per_cases")) {
    numerator <- strsplit(plot_what, "_per_")[[1]][1]
    denominator <- strsplit(plot_what, "_per_")[[1]][2]
    new_col_name <- paste0(numerator, "_per_", denominator)
    p <- data_grouped %>%
      tidyr::pivot_wider(names_from = value_type, values_from = value) %>%
      dplyr::mutate(value = .data[[numerator]] / .data[[denominator]]) %>%
      ggplot() +
        geom_line(aes(x = date, y = value)) +
        ylab(new_col_name)
  }
  
  # New cases/deaths plots
  if (plot_what %in% c("new_cases")) {
    value_type_col <- strsplit(plot_what, split = "_")[[1]][2]
    getNew <- function(vec) return (c(vec[1], diff(vec)))
    p <- data_grouped %>%
      tidyr::pivot_wider(names_from = value_type, values_from = value) %>%
      dplyr::mutate(value_raw = .data[[value_type_col]]) %>%
      dplyr::mutate(value_diff = getNew(.data$value_raw))
  }
  
}






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
