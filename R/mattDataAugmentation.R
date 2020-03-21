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
#' being the first date seen in the passed-in data frame in which the threshold number
#' of cases was reached in the value column of the data frame.
#' 
#' @param df a data frame for which this day of disease column will be created.
#' This data frame must contain a column labeled as "date" which is of type "Date"
#' @param threshold the number of cases required to state the "start" of the disease.
#' Default is 100, which conforms with epidemiology convention.
#' 
#' @return Output is the passed-in data frame with the addition of a day_of_disease column.
#' 
#' @importFrom lubridate is.Date
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr filter summarize mutate
#' 
#' @examples 
#' covid_data <- importCovidData()
#' dayOfDiseaseColumn(covid_data)
#' 
#' covid_us <- filterDiseaseData(covid_data, country = "US")
#' dayOfDiseaseColumn(covid_us)
#' 
#' zika_data <- importZikaData()
#' dayOfDiseaseColumn(zika_data)
#' 
#' sars_data <- importSARSData()
#' dayOfDiseaseColumn(sars_data)
#' 
#' @export
#'
dayOfDiseaseColumn <- function(df, threshold = 100) {
  if(!is.data.frame(df)){
    stop('Passed-in object must be of class \"data frame\"')
  } else if(!("date" %in% colnames(df))){
    stop('Passed-in data frame must have a \"date\" column')
  } else if(!lubridate::is.Date(df$date)){
    stop('The \"date\" column must be of class \"Date\"')
  }
  
  first_date = df %>%
    dplyr::filter(value_type == "cases" & value >= threshold) %>%
    dplyr::summarize(first_date_df = min(unique(date))) %>%
    magrittr::extract2(1)
  
  df = df %>%
    dplyr::mutate(day_of_disease = 1 + as.integer(date - as.Date(first_date)))
  return(df)
}



#' Congregation of Data Dates
#' 
#' Because the Zika data was tracked on different dates in different locations, this function
#' aligns and congregates the value counts on a weekly basis (taking the maximum if necessary) so that
#' the dates in the data are aligned for all locations. This function is only to be used on Zika data.
#' 
#' WARNING: This function manipulates the data in a way that makes the dates of observations 
#' less accurate, but in a way that is beneficial for plotting and understanding the cumulative
#' growth of the data.
#' 
#' @param df a data frame for which the data values will be congregated to weekly dates.
#' 
#' @return Output is a data frame of the same format as the passed-in df, but with congregated dates.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate slice n filter select
#' 
#' @examples
#' zika_raw <- importZikaData()
#' congregateDataDates(zika_raw)
#' 
#' @export
#'


congregateDataDates <- function(df) {
  # Check that this is Zika data
  if(length(unique(df$disease)) != 1 | unique(df$disease)[1] != "zika"){
    stop("This function is only to be used on Zika data.")
  }
  
  # Identify first and last dates
  dts = unique(df$date)
  first_date = as.Date(min(dts))
  last_date = (as.Date(max(dts)) + 7)

  # Sequence of weekly dates
  dates_used = seq(first_date, last_date, by = "weeks")
  
  # Determine all unique province-region combinations
  unique_locations = unique(df[, c('province', 'region', 'value_type', 'pop_2016', 'lat', 'long')])
  
  # Create data frame which has a row for each unique province-region-dates_used combination
  new_df = unique_locations %>%
    dplyr::mutate(disease = df$disease[1]) %>%
    dplyr::slice(rep(1:(dplyr::n()), each = length(dates_used))) %>%
    dplyr::mutate(date = as.Date(rep(dates_used, times = nrow(unique_locations))))
  
  #------------------------------------------------------------------
  small = function(r, df, new_df) {
    if(length(df$province) > 0){
      prov = new_df[r,"province"][[1]]
    }
    country = new_df[r,"region"][[1]]
    value_type = new_df[r,"value_type"][[1]]
    dt = as.Date(new_df[r,"date"][[1]])
    narrowed = df %>%
      dplyr::filter(region == country) %>%
      dplyr::filter(value_type == value_type) %>%
      dplyr::filter(date <= dt) %>%
      dplyr::filter(date >= (as.Date(dt) - 6))
    
    if(length(df$province) > 0) {
      narrowed = narrowed %>%
        dplyr::filter(province == prov)
    }
    
    value = 0
    if(nrow(narrowed) > 0) {
      value = max(narrowed$value)
    }
    return(value)
  }

  sapply(1:nrow(new_df), function(x) small(x, df, new_df))
  #------------------------------------------------------------------
  
  
  # for-loop which idenitifies the max value from prior week to fill into new data frame
  value_vec = c()
  for(i in 1:nrow(new_df)) {
    if(length(df$province) > 0){
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
    
    if(length(df$province) > 0) {
      narrowed = narrowed %>%
        dplyr::filter(province == prov)
    }
    
    value = 0
    if(nrow(narrowed) > 0) {
      value = max(narrowed$value)
    } else {
      # If no data from prior week, fill in data value from week before
      if(i > 1 && 
         new_df$province[i - 1] == prov && 
         new_df$region[i - 1] == country &&
         new_df$value_type[i - 1] == value_type) {
        value = value_vec[i - 1]
      }
    }
    value_vec = c(value_vec, value)
  }
  
  value_vec
  # Add value column and reformat data frame to match previous format
  new_df = new_df %>%
    dplyr::mutate(value = value_vec) %>%
    dplyr::select(disease, province, region, date, value, value_type, pop_2016, lat, long)

  return(new_df)
}

