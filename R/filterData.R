#' Filter Data Frame
#' 
#' Allows for easy filtering of the data by:
#' Disease - options are Coronavirus ("covid"), SARS ("sars"), and Zika ("zika"), defaults to Coronavirus 
#' Date - option to input first/last date
#' Region - Country/ies of interest
#' Province - Province(s) of interest within the country/ies of interest
#' Value Type - options are "cases", "deaths", and "recoveries", defaults to include all three
#' Value - option to input minimum/maximum values
#' 
#' @param disease The disease corresponding to the data that is to be filtered. 
#' The options are Coronavirus ("covid"), SARS ("sars"), and Zika ("zika"), and it 
#' defaults to Coronavirus if there is no input.
#' @param first_date The earliest date that is to be included in the returned data frame.
#' @param last_date The latest date that is to be included in the returned data frame.
#' @param country A vector of countries whose observations are to be included in the returned data frame.
#' @param province A vector of provinces within the countries included in the regions vector
#' whose observations are to be included in the returned data frame.
#' @param type A vector of value types whose obbservations are to be included in the returned
#' data frame. The options are "cases", "deaths", and "recoveries", and it defaults to include all
#' three if no input is given.
#' @param min_value The minimum value that is to be included in the returned data frame.
#' @param max_value The maximum value that is to be included in the returned data frame.
#' 
#' @return Output is the chosen data frame filtered to the chosen specifications.
#' 
#' @importFrom lubridate is.Date
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' 
#' @examples 
#' filterDiseaseData(country = c("Japan", "Italy", "US"))
#' filterDiseaseData("sars", value_type = c("cases", "deaths"), min_value = 10)
#' filterDiseaseData("zika", first_date = "2016-05-01", last_date = "2016-05-31")
#' 
#' @export
#'
filterDiseaseData <- function(data = NA, disease = c("covid", "sars", "zika"), first_date = NA, last_date = NA, 
                  country = c(), province = c(), type = c("cases", "deaths", "recoveries"),
                  min_value = 0, max_value = Inf) {
  
  if(!is.na(data)) {
    df = data
  }
  
  disease = match.arg(disease)
  
  if(disease == "covid") {
    df = importCovidData()
  } else if(disease == "sars") {
    df = importSARSData()
  } else if(disease == "zika") {
    df = importZikaData()
  }
  
  if(is.na(first_date)) {
    first_date = min(df$date, na.rm = TRUE)
  }
  if(is.na(last_date)) {
    last_date = max(df$date, na.rm = TRUE)
  }
  if(length(country) == 0) {
    country = unique(df$region)
  }
  if(length(province) == 0 & ("province" %in% colnames(df))) {
    province = unique(df$province)
  }
  
  df = df %>%
    dplyr::filter(date >= as.Date(first_date) & date <= as.Date(last_date)) %>%
    dplyr::filter(region %in% country) %>%
    dplyr::filter(value_type %in% type) %>%
    dplyr::filter(value >= min_value & value <= max_value)
  
  if("province" %in% colnames(df)) {
    df = df %>%
      dplyr::filter(province %in% province)
  }
  
  return(df)
}



