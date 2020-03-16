
covid_data <- importCovidData()
us_covid_data <- filterDiseaseData(covid_data, country = "US")
china_covid_data <- filterDiseaseData(covid_data, country = "China")
japan_covid_data <- filterDiseaseData(covid_data, country = "Japan")

#' Create Time-Series Plots
#'
#' Visualize disease statistics on a day-by-day basis
#'
#' @param data disease data to plot (usually the output of filterDiseaseData)
#' @param plot_what options are: "cases", "recovered", "deaths" (plots totals by date),
#'   "log_cases", "log_recovered", "log_deaths" (plots log of totals by date),
#'   "x_per_y" where x and y are either "cases", "recovered", "deaths", or "pop_2018" (plots the ratio by date),
#'   "new_cases", "new_recovered", "new_deaths" (plots change by date) or 
#'   "growth_factor" (plots new cases divided by new cases on previous date for each date),
#'   if plot_what is not specified, plots cases, recovered, and deaths by day
#'   
#' @import ggplot2
#' @importFrom dplyr group_by summarize filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' 
#' @examples 
#' covid_data <- importCovidData()
#' us_covid_data <- filterDiseaseData(covid_data, country = "US")
#' plotTimeSeries(us_covid_data)
#' plotTimeSeries(us_covid_data, plot_what = "cases")
#' plotTimeSeries(us_covid_data, plot_what = "log_cases")
#' plotTimeSeries(us_covid_data, plot_what = "deaths_per_cases")
#' plotTimeSeries(us_covid_data, plot_what = "recovered_per_cases")
#' plotTimeSeries(us_covid_data, plot_what = "cases_per_pop_2018")
#' plotTimeSeries(us_covid_data, plot_what = "new_cases")
#' plotTimeSeries(us_covid_data, plot_what = "new_deaths")
#' plotTimeSeries(us_covid_data, plot_what = "growth_factor")
#' 
#' @export 
#' 
plotTimeSeries <- function(data, plot_what = "cases") {
  ## TODO: add axis labels, title, and color to the plots 
  ## TODO: add group arg that is either "province" "region" or "all" group to that
  ##    ggplot() + geom_point(aes(x = date, y = value, col = province))
  ## TODO: add option to make x-axis days since first case 
  
  ## Check the plot_what, make sure data can support that plot
  ## If not, throw an error saying the problem
  isPlotWhatAcceptable <- function(data_arg, plot_what_arg) {
    # returns "" if plot_what argument is acceptable, an informative error if not
    if (!("value_type" %in% names(data_arg))) {
      return ("data must have 'value_type' column")
    }
    if (grepl("log_", plot_what_arg)) {
      column <- strsplit(plot_what_arg, split = "log_")[[1]][2]
      if (!(column %in% data_arg$value_type)) {
        return (paste0("'value_type' column of data must include ", column))
      }
    } else if (grepl("_per_", plot_what_arg)) {
      columns <- strsplit(plot_what_arg, split = "_per_")[[1]]
      if (!all(columns %in% c(data_arg$value_type, "pop_2018"))) {
        return ("must have an element of 'value_type' column of data or 'pop_2018' on either side of '_per_'")
      }
    } else if (grepl("new_", plot_what_arg)) {
      value_type_to_diff <- strsplit(plot_what_arg, split = "new_")[[1]][2]
      if (sum(data_arg$value_type == value_type_to_diff) < 1) {
        if (sum(data_arg$value_type == "cases") == 0) {
          return (paste0("'value_type' column of data must include '", value_type_to_diff, "'"))
        }
        return ("need more days to calculate differences by day")
      }
    } else if (plot_what_arg == "growth_factor") {
      if (sum(data_arg$value_type == "cases") < 2) {
        if (sum(data_arg$value_type == "cases") == 0) {
          return ("'value_type' column of data must include 'cases'")
        }
        return ("need more days to calculate growth factor")
      }
    } else {
      # plot_what_arg is "cases", "recovered", or "deaths"
      if (!(plot_what_arg %in% data_arg$value_type)) {
        return (paste0("'value_type' column of data must include ", plot_what_arg))
      }
    }
    return ("")
  }
  error_msg <- isPlotWhatAcceptable(data, plot_what)
  if (error_msg != "") {
    stop (paste0("(check plotTimeSeries documentation): ", error_msg))
  }
  
  ## if there is more than one province in the data, it groups to region level
  ## if there is more than one region in the data, it groups to world level
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
  
  if (plot_what %in% c("cases", "deaths", "recovered")) {
    p <- data_grouped %>%
      dplyr::filter(value_type == plot_what) %>%
      ggplot() +
        geom_line(aes(x = date, y = value))
    return (p)
  }
  
  if (plot_what %in% c("log_cases", "log_deaths", "log_recovered")) {
    value_type_to_log <- strsplit(plot_what, split = "log_")[[1]][2]
    p <- data_grouped %>%
      dplyr::ungroup() %>%
      dplyr::filter(value_type == value_type_to_log) %>%
      dplyr::mutate(value = log(value)) %>%
      ggplot() +
        geom_line(aes(x = date, y = value))
    return (p)
  }
  
  # for plot_what = deaths / cases, recovered / cases, cases / pop_2018, etc.
  if (grepl("_per_", plot_what)) {
    numerator <- strsplit(plot_what, "_per_")[[1]][1]
    denominator <- strsplit(plot_what, "_per_")[[1]][2]
    new_col_name <- paste0(numerator, "_per_", denominator)
    p <- data_grouped %>%
      tidyr::pivot_wider(names_from = value_type, values_from = value) %>%
      dplyr::mutate(value = .data[[numerator]] / .data[[denominator]]) %>%
      ggplot() +
        geom_line(aes(x = date, y = value)) +
        ylab(new_col_name)
    return (p)
  }
  
  # new_ plots
  if (grepl("new_", plot_what)) {
    value_type_col <- strsplit(plot_what, split = "_")[[1]][2]
    getDiffs <- function(vec) return (c(vec[1], diff(vec)))
    getDiffsCol <- function(data, col_name) {
      column <- data[[col_name]]
      getDiffs(column)
    }
    p <- data_grouped %>%
      tidyr::pivot_wider(names_from = value_type, values_from = value) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = getDiffsCol(.data, value_type_col)) %>%
      ggplot() +
        geom_point(aes(x = date, y = value))
    return (p)
  }
  
  # growth_factor: new cases today / new cases yesterday 
  if (plot_what == "growth_factor") {
    getDiffs <- function(vec) return (c(vec[1], diff(vec)))
    getPctChange <- function(vec) {
      lagged_vec <- c(NA, vec[-length(vec)])
      lagged_vec[lagged_vec == 0] <- NA
      return (vec / lagged_vec)
    }
    p <- data_grouped %>%
      tidyr::pivot_wider(names_from = value_type, values_from = value) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diffs = getDiffs(cases)) %>%
      dplyr::mutate(value = getPctChange(diffs)) %>%
      ggplot() +
        geom_point(aes(x = date, y = value))
    return (p)
  }
}

plotTimeSeries(data = us_covid_data, plot_what = "default")
plotTimeSeries(data = us_covid_data, plot_what = "cases")
plotTimeSeries(data = us_covid_data, plot_what = "recovered")
plotTimeSeries(data = us_covid_data, plot_what = "deaths")
plotTimeSeries(data = china_covid_data, plot_what = "log_cases")
plotTimeSeries(data = china_covid_data, plot_what = "log_recovered")
plotTimeSeries(data = china_covid_data, plot_what = "log_deaths")
plotTimeSeries(data = china_covid_data, plot_what = "cases_per_pop_2018")
plotTimeSeries(data = china_covid_data, plot_what = "deaths_per_cases")
plotTimeSeries(data = china_covid_data, plot_what = "deaths_per_pop_2018")
plotTimeSeries(data = japan_covid_data, plot_what = "new_cases")
plotTimeSeries(data = japan_covid_data, plot_what = "new_recovered")
plotTimeSeries(data = japan_covid_data, plot_what = "new_deaths")
plotTimeSeries(data = japan_covid_data, plot_what = "growth_factor")
