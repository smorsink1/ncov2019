
#' Imports Coordinate Data
#' 
#' Imports country latitude and longitude data from Google public dataset
#' 
#' @return Output is a dataframe with columns for country (character), 
#' latitude (double), and longitude (double).
#' 
#' @importFrom xml2 read_html
#' @importFrom magrittr %>% extract2
#' @importFrom rvest html html_nodes html_table
#' @importFrom dplyr mutate select rename
#' 
#' @examples 
#' importCoordinateData()
#' 
#' @export
#' 
importCoordinateData <- function() {
  url = "https://developers.google.com/public-data/docs/canonical/countries_csv"
  coordinates = url %>%
    xml2::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::mutate(latitude = as.double(latitude)) %>%
    dplyr::mutate(longitude = as.double(longitude)) %>%
    dplyr::select(name, latitude, longitude) %>%
    dplyr::rename(region = name)
  return(coordinates)
}

#' Adding Day of Disease Column
#' 
#' Adds a column of the data frame that gives the day of the disease, with day 1
#' being the first date seen in the passed-in data frame
#' 
#' @param df a data frame for which this day of disease column will be created.
#' This data frame must contain a column labeled as "date" which is of type "Date"
#' 
#' @return Output is the passed-in data frame with the addition of a day_of_disease column.
#' 
#' @importFrom lubridate is.Date
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr filter summarize mutate
#' 
#' @examples 
#' dayOfDiseaseColumn(importSARSData())
#' dayOfDiseaseColumn(importCovidData())
#' dayOfDiseaseColumn(importZikaData())
#' 
#' @export
#'
dayOfDiseaseColumn <- function(df) {
  if(!is.data.frame(df)){
    stop('Passed-in object must be of class \"data frame\"')
  } else if(!("date" %in% colnames(df))){
    stop('Passed-in data frame must have a \"date\" column')
  } else if(!lubridate::is.Date(df$date)){
    stop('The \"date\" column must be of class \"Date\"')
  }
  
  first_date = df %>%
    dplyr::filter(value_type == "cases" & value > 0) %>%
    dplyr::summarize(first_date_df = min(unique(date))) %>%
    magrittr::extract2(1)
  
  df = df %>%
    dplyr::mutate(day_of_disease = 1 + as.integer(date - as.Date(first_date)))
  return(df)
}






# Time = 25 seconds for weekly Zika
congregateDataDates <- function(df, frequency = "weekly") {
  dts = unique(df$date)
  first_date = as.Date(min(dts))
  last_date = (as.Date(max(dts)) + 7)
  
  if(frequency == "weekly") {
    time = "weeks"
  } else if(frequency == "daily") {
    time = "days"
  }
  
  dates_used = seq(first_date, last_date, by = time)
  
  unique_locations = unique(df[, c('province', 'region', 'value_type', 'pop_2016', 'lat', 'long')])
  
  new_df = unique_locations %>%
    dplyr::mutate(disease = df$disease[1]) %>%
    dplyr::slice(rep(1:n(), each = length(dates_used))) %>%
    dplyr::mutate(date = as.Date(rep(dates_used, times = nrow(unique_locations))))
  
  value_vec = c()
  for(i in 1:nrow(new_df)) {
    if(exists(df$province)){
      prov = new_df[i,"province"][[1]]
    }
    country = new_df[i,"region"][[1]]
    value_type = new_df[i,"value_type"][[1]]
    dt = as.Date(new_df[i,"date"][[1]])
    narrowed = df %>%
      dplyr::filter(region == country) %>%
      dplyr::filter(value_type == value_type) %>%
      dplyr::filter(date <= dt) %>%
      dplyr::filter(date >= (as.Date(dt) - 6))
    
    if(exists(prov)) {
      narrowed = narrowed %>%
        dplyr::filter(province == prov)
    }
    
    value = 0
    if(nrow(narrowed) > 0) {
      value = max(narrowed$value)
    } else {
      if(i > 1 && 
         new_df$province[i - 1] == prov && 
         new_df$region[i - 1] == country &&
         new_df$value_type[i - 1] == value_type) {
        value = value_vec[i - 1]
      }
    }
    value_vec = c(value_vec, value)
  }
  new_df %>%
    dplyr::select(disease, province, region, date, value_type, pop_2016, lat, long)
  
  new_df$value = value_vec
  
  return(new_df)
}

x = congregateDataDates(zika_confirmed)
