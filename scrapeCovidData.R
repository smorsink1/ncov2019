
#' Scrape coronavirus time series data
#'
#' Scrapes the Johns Hopkins Github to get day-by-day coronavirus data
#' 
#' @param 
#' @param 
#' 
#' @export 
#' 
#' # import from packages: 
#' # <at>importFrom magrittr %>% 
#' @importFrom readr read_csv
#' 
#' # Examples like this: <at>examples
#'
#'
scrapeCovidData <- function() {
  link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  data <- readr::read_csv(link)
}


