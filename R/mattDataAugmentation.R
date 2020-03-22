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
#' This function aligns and congregates data value counts on a weekly basis (taking the maximum if necessary)
#' so that the dates in the data are aligned for all data value types at all locations.
#' 
#' WARNING: This function manipulates the data in a way that makes the dates of observations 
#' less accurate, but in a way that is beneficial for plotting and understanding the cumulative
#' growth of the data.
#' 
#' @param df A data frame for which the data values will be congregated to weekly dates.
#' 
#' @return Output is a data frame of the same format as the passed-in df, but with congregated dates.
#' 
#' @importFrom purrr map pmap_dfr
#' @importFrom tibble tibble 
#' @importFrom dplyr left_join select rename distinct slice mutate n
#'  
#' @examples
#' zika_raw <- importZikaData()
#' congregateDataDates(zika_raw)
#' 
#' @export
#'

congregateDataDates <- function(df) {
  # Check that this is all the same disease 
  if (length(unique(df$disease)) != 1) {
    stop("'disease' column has multiple entries, function can only be used on one disease")
  }
  pop_col_name <- names(df)[grepl("pop_", names(df))]
  names(df)[grepl("pop_", names(df))] <- "pop"
  
  # Identify first and last dates
  first_date = min(as.Date(df$date))
  last_date = max(as.Date(df$date)) + 7
  # Sequence of weekly dates
  dates_used = seq(first_date, last_date, by = "weeks")
  
  if ("province" %in% names(df)) {
    map_df <- df[!duplicated(df[, c("province", "region", "value_type")]), ]
  } else {
    map_df <- df[!duplicated(df[, c("region", "value_type")]), ]
  }
  
  map_df_upd <- map_df %>%
    dplyr::slice(rep(1:(dplyr::n()), each = length(dates_used))) %>%
    dplyr::mutate(congregate_date = as.Date(dates_used))
  
  getCongregateValue <- function(date_congregate, type, reg, prov) {
    date_congregate <- as.Date(date_congregate)
    subset <- df[(df$date >= first_date & df$date <= date_congregate) & df$value_type == type & df$region == reg, ]
    if (!is.na(prov)) {
      subset <- subset[subset$province == prov, ]
    }
    max_value <- suppressWarnings(max(max(subset$value), 0))
    result <- tibble::tibble("congregate_date" = as.character(date_congregate), "value_type" = type, "region" = reg, "value" = max_value)
    if (!is.na(prov)) {
      result[["province"]] <- prov
    }
    return (result)
  }
  
  if ("province" %in% names(df)) {
    congregated <- purrr::pmap_dfr(list(map_df_upd$congregate_date,
                                        map_df_upd$value_type,
                                        map_df_upd$region,
                                        map_df_upd$province), getCongregateValue)
  } else {
    congregated <- purrr::pmap_dfr(list(map_df_upd$congregate_date,
                                        map_df_upd$value_type,
                                        map_df_upd$region,
                                        rep(NA, nrow(map_df_upd))), getCongregateValue)
  }
  congregated$congregate_date <- as.Date(congregated$congregate_date)
  
  if ("province" %in% names(df)) {
    joined <- congregated %>%
      dplyr::left_join(df, by = c("value_type" = "value_type",
                                  "region" = "region",
                                  "province" = "province"))
  } else {
    joined <- congregated %>%
      dplyr::left_join(df, by = c("value_type" = "value_type",
                                  "region" = "region"))
  }
  final <- joined %>%
    dplyr::select(-value.y, -date) %>%
    dplyr::rename("value" = "value.x", "date" = "congregate_date") %>%
    dplyr::select(disease, province, region, date, value, value_type, pop, lat, long) %>%
    dplyr::rename(!!pop_col_name := "pop") %>%
    dplyr::distinct()
  return (final)
}

