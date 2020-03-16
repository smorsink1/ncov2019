#' Filter Data Frame
#' 
#' Allows for easy filtering of the data by:
#' Disease - options are Coronavirus ("covid"), SARS ("sars"), and Zika ("zika"), defaults to Coronavirus 
#' Date - option to input first/last date
#' Region - Country/ies of interest
#' Province - Province(s) of interest within the country/ies of interest
#' Value Type - options are "cases", "deaths", and "recovered", defaults to include all three
#' Value - option to input minimum/maximum values
#' 
#' @param data A data frame to be filtered (optional input)
#' @param disease The disease corresponding to the data that is to be filtered. 
#' The options are Coronavirus ("covid"), SARS ("sars"), and Zika ("zika"), and it 
#' defaults to Coronavirus if there is no input.
#' @param first_date The earliest date that is to be included in the returned data frame.
#' @param last_date The latest date that is to be included in the returned data frame.
#' @param country A vector of countries whose observations are to be included in the returned data frame.
#' @param province A vector of provinces within the countries included in the regions vector
#' whose observations are to be included in the returned data frame.
#' @param type A vector of value types whose obbservations are to be included in the returned
#' data frame. The options are "cases", "deaths", and "recovered", and it defaults to include all
#' three if no input is given.
#' @param min_value The minimum value that is to be included in the returned data frame.
#' @param max_value The maximum value that is to be included in the returned data frame.
#' @param include_suspected Only applicable if disease = "zika". Boolean that determines whether suspected
#' cases should be included in cases. Default is FALSE.
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
filterDiseaseData <- function(data, first_date = NA, last_date = NA, 
                  country = c(), province = c(), type = c("cases", "deaths", "recovered"),
                  min_value = 0, max_value = Inf, include_suspected = FALSE) {
  
  if("cumulative_confirmed_cases" %in% data$value_type | 
     "cumulative_suspected_cases" %in% data$value_type) {
    if(include_suspected) {
      data$value_type = "cases"
    } else {
      data = data %>% dplyr::filter(value_type == "cumulative_confirmed_cases")
      data$value_type = "cases"
    }
  }
  
  if(!is.na(first_date) & !(as.Date(first_date) %in% unique(data$date))) {
    stop(paste0("First date must be in the data set, which contains dates ", min(unique(data$date)), " to ", max(unique(data$date)), "."))
  }
  if(!is.na(last_date) & !(as.Date(last_date) %in% unique(data$date))) {
    stop(paste0("Last date must be in the data set, which contains dates ", min(unique(data$date)), " to ", max(unique(data$date)), "."))
  }
  if(!is.na(first_date) & !is.na(last_date) & as.Date(first_date) > as.Date(last_date)) {
    stop("First date cannot be later than last date.")
  }
  
  if(length(country) > 0 && !(country %in% data$region)){
    i = !(country %in% data$region)
    warning(paste0('The country \"', country[i], '\" was not found in the data set. \n'))
  }
  
  if(length(province) > 0 && !(province %in% data$province)){
    i = !(province %in% data$province)
    warning(paste0('The province \"', province[i], '\" was not found in the data set. \n'))
  }
  
  if(!(is.numeric(min_value) & is.numeric(max_value) & min_value >= 0 & max_value >= 0)) {
    stop("Minimum value and maximum value must be positive numeric values.")
  }
  
  if(min_value > max_value) {
    stop("Minimum value must be less than maximum value.")
  }
    
  if(is.na(first_date)) {
    first_date = min(data$date, na.rm = TRUE)
  }
  if(is.na(last_date)) {
    last_date = max(data$date, na.rm = TRUE)
  }
  if(length(country) == 0) {
    country = unique(data$region)
  }
  if(length(province) == 0 & ("province" %in% colnames(data))) {
    province = unique(data$province)
  }
  
  data = data %>%
    dplyr::filter(date >= as.Date(first_date) & date <= as.Date(last_date)) %>%
    dplyr::filter(region %in% country) %>%
    dplyr::filter(value_type %in% type) %>%
    dplyr::filter(value >= min_value & value <= max_value)
  
  if("province" %in% colnames(data)) {
    data = data %>%
      dplyr::filter(province %in% province)
  }
  
  return(data)
}



