
#' Create Time-Series Plots
#'
#' Visualize disease statistics on a day-by-day basis
#'
#' @param data disease data to plot (usually the output of filterDiseaseData)
#' @param plot_what options are: "cases", "recovered", "deaths" (plots totals by date),
#'   "log_cases", "log_recovered", "log_deaths" (plots log of totals by date),
#'   "x_per_y" where x and y are either "cases", "recovered", "deaths", or "pop" (plots the ratio by date),
#'   "new_cases", "new_recovered", "new_deaths" (plots change by date) or 
#'   "growth_factor" (plots new cases divided by new cases on previous date for each date),
#'   if plot_what is not specified, plots cases, recovered, and deaths by day
#' @param group options are: "province", "region", or "all" ("all" is the default).
#'   "province" creates one plot layer for each province in the data,
#'   "region" creates one plot layer for each region in the data,
#'   "all" creates one plot layer total
#' @param x_axis options are "date" ("date" is the default) or "day_of_disease".
#'   "date" makes the x-axis the calendar date,
#'   "day_of_disease" makes the x-axis the days since the first case in the group
#'   (where group is specified in the group argument)
#'   
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom dplyr group_by summarize filter distinct pull ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' 
#' @examples 
#' covid_data <- importCovidData()
#' us_covid_data <- filterDiseaseData(covid_data, country = "US")
#' plotTimeSeries(us_covid_data)
#' plotTimeSeries(us_covid_data, plot_what = "cases")
#' plotTimeSeries(us_covid_data, plot_what = "log_cases")
#' plotTimeSeries(us_covid_data, plot_what = "deaths_per_cases")
#' plotTimeSeries(us_covid_data, plot_what = "recovered_per_cases")
#' plotTimeSeries(us_covid_data, plot_what = "cases_per_pop")
#' plotTimeSeries(us_covid_data, plot_what = "new_cases")
#' plotTimeSeries(us_covid_data, plot_what = "new_deaths")
#' plotTimeSeries(us_covid_data, plot_what = "growth_factor")
#' 
#' @export 
#' 
plotTimeSeries <- function(data, plot_what = "cases", group = "all", x_axis = "date") {
  names(data)[grepl("pop", names(data))] <- "pop"
  if (!(group %in% c("all", "province", "region"))) {
    stop ("'group' argument must be 'all', 'province', or 'region'")
  }
  if (!(x_axis %in% c("date", "day_of_disease"))) {
    stop("'x_axis' argument must be 'date' or 'day_of_disease'")
  }
  # Check the plot_what, make sure data can support that plot
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
      if (!all(columns %in% c(data_arg$value_type, "pop"))) {
        return ("must have an element of 'value_type' column of data or some string containing 'pop' on either side of '_per_'")
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
  ## group the data depending on the group argument
  if (group == "all") {
    total_pop <- data %>%
      dplyr::distinct(region, .keep_all = TRUE) %>%
      dplyr::pull(pop) %>%
      sum()
    data_grouped <- data %>%
      dplyr::group_by(date, value_type) %>%
      dplyr::summarize(pop = total_pop, value = sum(value))
    title <- "All"
  } else if (group == "province") {
    if (grepl("pop", plot_what)) {
      message("population data not available at province level, using region population data")
    }
    # province level
    data_grouped <- data %>%
      dplyr::group_by(date, pop, region, province, value_type) %>%
      dplyr::summarize(value = sum(value))
    title <- "Province"
  } else {
    # region level
    data_grouped <- data %>%
      dplyr::group_by(date, pop, region, value_type) %>%
      dplyr::summarize(value = sum(value))
    title <- "Region"
  }
  if (x_axis == "day_of_disease") {
    if (group == "all") {
      data_grouped <- data_grouped %>%
        dplyr::ungroup() %>%
        dayOfDiseaseColumn()
    } else {
      # group_by %>% group_modify does not work
      data_grouped <- data_grouped %>%
        dplyr::ungroup() %>%
        split(data_grouped[[group]]) %>%
        purrr::map_dfr(dayOfDiseaseColumn) %>%
        dplyr::filter(day_of_disease >= 1)
    }
  }
  if (group == "all") {
    group <- ""
  }
  if (plot_what %in% c("cases", "deaths", "recovered")) {
    p <- data_grouped %>%
      dplyr::filter(value_type == plot_what) %>%
      ggplot() +
        geom_line(aes(x = !!rlang::sym(x_axis), y = value, col = !!rlang::sym(group))) +
        xlab(x_axis) + ylab(plot_what) + 
        ggtitle(paste0("By ", title, ": ", plot_what))
    return (p)
  }
  
  if (plot_what %in% c("log_cases", "log_deaths", "log_recovered")) {
    value_type_to_log <- strsplit(plot_what, split = "log_")[[1]][2]
    p <- data_grouped %>%
      dplyr::ungroup() %>%
      dplyr::filter(value_type == value_type_to_log) %>%
      dplyr::mutate(value = replace(value, value == 0, 1)) %>%
      dplyr::mutate(value = log(value)) %>%
      ggplot() +
        geom_line(aes(x = !!rlang::sym(x_axis), y = value, col = !!rlang::sym(group))) +
        xlab(x_axis) + ylab(plot_what) + 
        ggtitle(paste0("By ", title, ": ", plot_what))
    return (p)
  }
  
  # for plot_what = deaths / cases, recovered / cases, cases / pop, etc.
  if (grepl("_per_", plot_what)) {
    numerator <- strsplit(plot_what, "_per_")[[1]][1]
    denominator <- strsplit(plot_what, "_per_")[[1]][2]
    new_col_name <- paste0(numerator, "_per_", denominator)
    p <- data_grouped %>%
      tidyr::pivot_wider(names_from = value_type, values_from = value) %>%
      dplyr::mutate(value = .data[[numerator]] / .data[[denominator]]) %>%
      ggplot() +
        geom_line(aes(x = !!rlang::sym(x_axis), y = value, col = !!rlang::sym(group))) +
        xlab(x_axis) + ylab(new_col_name) + 
        ggtitle(paste0("By ", title, ": ", new_col_name))
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
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::mutate(value = getDiffsCol(.data, value_type_col)) %>%
      ggplot() +
        geom_point(aes(x = !!rlang::sym(x_axis), y = value, col = !!rlang::sym(group))) +
        xlab(x_axis) + ylab(plot_what) + 
        ggtitle(paste0("By ", title, ": ", plot_what))
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
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::mutate(diffs = getDiffs(cases)) %>%
      dplyr::mutate(value = getPctChange(diffs)) %>%
      ggplot() +
        geom_point(aes(x = !!rlang::sym(x_axis), y = value, col = !!rlang::sym(group))) +
        ylim(0, 2) + xlab(x_axis) + ylab(plot_what) +
        ggtitle(paste0("By ", title, ": ", plot_what))
    return (p)
  }
}

covid_data <- importCovidData()
covid_data_america <- importCovidData() %>%
  dplyr::filter(region %in% c("US", "Mexico", "Canada"))
zika_data <- importZikaData()
zika_data_subset <- filterDiseaseData(zika_data, country = "Colombia", include_suspected = T) %>%
  dplyr::bind_rows(filterDiseaseData(zika_data, country = "El_Salvador", include_suspected = T))
sars_data <- importSARSData()  
sars_data_subset <- filterDiseaseData(sars_data, country = "Singapore") %>%
  dplyr::bind_rows(filterDiseaseData(sars_data, country = "China"))

covid_data_subset <- covid_data %>%
  filterDiseaseData(country = "US") %>%
  dplyr::bind_rows(filterDiseaseData(covid_data, country = "China")) %>%
  dplyr::bind_rows(filterDiseaseData(covid_data, country = "Italy")) %>%
  dplyr::bind_rows(filterDiseaseData(covid_data, country = "Iran")) %>%
  dplyr::bind_rows(filterDiseaseData(covid_data, country = "Singapore"))

plotTimeSeries(covid_data_subset, plot_what = "log_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(covid_data_subset, plot_what = "deaths_per_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(covid_data_subset, plot_what = "new_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(covid_data_subset, plot_what = "growth_factor", x_axis = "day_of_disease")
plotTimeSeries(covid_data, plot_what = "growth_factor", x_axis = "day_of_disease")

plotTimeSeries(covid_data_america, plot_what = "cases", group = "all", x_axis = "date")
plotTimeSeries(covid_data_america, plot_what = "cases", group = "all", x_axis = "day_of_disease")
plotTimeSeries(data = covid_data_america, plot_what = "deaths", group = "region", x_axis = "date")
plotTimeSeries(data = covid_data_america, plot_what = "deaths", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = covid_data_america, plot_what = "deaths_per_cases", group = "region")
plotTimeSeries(data = covid_data_america, plot_what = "deaths_per_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = covid_data_america, plot_what = "log_cases", group = "region")
plotTimeSeries(data = covid_data_america, plot_what = "log_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = covid_data_america, plot_what = "cases_per_pop", group = "region")
plotTimeSeries(data = covid_data_america, plot_what = "cases_per_pop", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = covid_data_america, plot_what = "new_cases", group = "region")
plotTimeSeries(data = covid_data_america, plot_what = "new_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = covid_data_america, plot_what = "growth_factor", group = "all")
plotTimeSeries(data = covid_data_america, plot_what = "growth_factor", x_axis = "day_of_disease")

plotTimeSeries(zika_data_subset, plot_what = "cases", group = "all", x_axis = "date")
plotTimeSeries(zika_data_subset, plot_what = "cases", group = "all", x_axis = "day_of_disease")
plotTimeSeries(data = zika_data_subset, plot_what = "cases", group = "region", x_axis = "date")
plotTimeSeries(data = zika_data_subset, plot_what = "deaths", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = zika_data_subset, plot_what = "cases_per_pop", group = "region")
plotTimeSeries(data = zika_data_subset, plot_what = "deaths_per_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = zika_data_subset, plot_what = "log_cases", group = "region")
plotTimeSeries(data = zika_data_subset, plot_what = "log_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = zika_data_subset, plot_what = "cases_per_pop", group = "region")
plotTimeSeries(data = zika_data_subset, plot_what = "cases_per_pop", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = zika_data_subset, plot_what = "new_cases", group = "region")
plotTimeSeries(data = zika_data_subset, plot_what = "new_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = zika_data_subset, plot_what = "growth_factor", group = "all")
plotTimeSeries(data = zika_data_subset, plot_what = "growth_factor", x_axis = "day_of_disease")

plotTimeSeries(sars_data_subset, plot_what = "cases", group = "all", x_axis = "date")
plotTimeSeries(sars_data_subset, plot_what = "cases", group = "all", x_axis = "day_of_disease")
plotTimeSeries(data = sars_data_subset, plot_what = "cases", group = "region", x_axis = "date")
plotTimeSeries(data = sars_data_subset, plot_what = "deaths", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = sars_data_subset, plot_what = "cases_per_pop", group = "region")
plotTimeSeries(data = sars_data_subset, plot_what = "deaths_per_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = sars_data_subset, plot_what = "log_cases", group = "region")
plotTimeSeries(data = sars_data_subset, plot_what = "log_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = sars_data_subset, plot_what = "cases_per_pop", group = "region")
plotTimeSeries(data = sars_data_subset, plot_what = "deaths_per_pop", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = sars_data_subset, plot_what = "new_cases", group = "region")
plotTimeSeries(data = sars_data_subset, plot_what = "new_cases", group = "region", x_axis = "day_of_disease")
plotTimeSeries(data = sars_data_subset, plot_what = "growth_factor", group = "all")
plotTimeSeries(data = sars_data_subset, plot_what = "growth_factor", x_axis = "day_of_disease")
