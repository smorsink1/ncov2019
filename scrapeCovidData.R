
#' Downloads Time-Series Coronavirus Data
#'
#' Imports data from public Johns Hopkins CSSEGISandData github repo on
#'   day-by-day coronavirus confirmed cases, deaths, and recoveries by location
#' 
#' @param type a string representing the type of coronavirus data desired:
#'   either "cases" for confirmed cases (default), "deaths" for deaths,
#'   or "recovered" for recoveries
#' 
#' @return Output is a dataframe with rows representing locations,
#'   columns representing dates, values representing the number of people 
#'
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' 
#' @examples
#' importCovidData()
#' importCovidData("cases")
#' importCovidData("deaths")
#' importCovidData("recovered") 
#' 
scrapeCovidData <- function(type = "cases") {
  if (!(type %in% c("cases", "deaths", "recovered"))) {
    stop ("type must be either cases (for confirmed cases), deaths, or recovered")
  }
  cases_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  deaths_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  recovered_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
  # accessing the correct link given the argument
  link <- paste0(type, "_link") %>%
    parse(text = .) %>%
    eval()
  return (readr::read_csv(link))
}

#' Tidies up scraped coronavirus data
#' 
#' Pivots the data frame to make one row per location/date combo
#' 
#' @param covid_df a time-series dataframe of the type scraped from 
#'   JHUCSSEGISandData github repo (columns for dates, rows for locations)
#' @param covid_df_name a character string representing the type of coronavirus data:
#'   ie either "cases", "deaths", or "recovered"
#' 
#' @return Output is a dataframe with columns for province, region, lat, long,
#'   date, value, and value_type
#' 
#' @importFrom tidyr pivot_longer
#' 
#' @examples 
#' tidyCovidData(scrapeCovidData())
#' tidyCovidData(scrapeCovidData("cases"))
#' 
tidyCovidData <- function(covid_df, covid_df_name) {
  # detecting date columns, pivoting the data to be longer
  col_names <- names(covid_df)
  col_names_char1 <- substr(col_names, 1, 1)
  date_cols_index <- grep("\\d", col_names_char1)
  covid_df_tidy <- tidyr::pivot_longer(data = covid_df, cols = date_cols_index, 
                                       names_to = "date", values_to = "value")
  # cleaning the date column
  covid_df_tidy$date <- as.Date(covid_df_tidy$date, format = "%m/%d/%y")
  # renaming
  names(covid_df_tidy)[1:4] <- c("province", "region", "lat", "long")
  # adding column for value type
  covid_df_tidy$value_type <- covid_df_name
  return (covid_df_tidy)
}

#' Imports Tidy Coronavirus Data
#' 
#' Imports data from public Johns Hopkins CSSEGISandData github repo on coronavirus
#'   confirmed cases, deaths, and recoveries by location, then collects all information
#'   in a tidy dataset
#' 
#' @return Output is a dataframe with columns for date (Date), location (character),
#'   value_type (character; either "cases", "deaths", or "recoveries"), and value (int)
#'
#' @importFrom dplyr bind_rows
#' 
#' @examples 
#' importCovidData()
#' 
#' @export
importCovidData <- function() {
  types <- c("cases", "deaths", "recovered")
  covid_data <- suppressMessages(lapply(types, scrapeCovidData))
  names(covid_data) <- types
  covid_data_tidy <- mapply(tidyCovidData, covid_data, names(covid_data),
                            SIMPLIFY = FALSE)
  return (dplyr::bind_rows(covid_data_tidy))
}
