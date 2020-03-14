#' Scrapes SARS disease data
#'
#' Scrapes the Kaggle SARS disease data from a public Github repository.
#' 
#' @return Output is a raw dataframe in its untidy format, directly from
#'  the Github repository
#'  
#' @importFrom readr read_csv
#' 
#' @examples 
#' scrapeSARSData()
#' 
#' @export
#'
scrapeSARSData <- function() {
  link <- "https://raw.githubusercontent.com/mcolon21/ncov2019data/master/sars_data.csv"
  data <- tryCatch(suppressMessages(readr::read_csv(link)),
                   error = function(e) stop ("Data no longer found at url"))
  return(data)
}


#' Loads and cleans SARS disease data
#'
#' Tidies and imports the SARS disease data to conform to the tidy format,
#'   does NOT include population data or latitude/longitude data
#' 
#' @return Output is a dataframe with columns for date (Date), region (character),
#'   value_type (character; either "cases", "deaths", or "recovered"), and value (int)
#'   
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr rename select arrange
#' 
#' @examples
#' cleanSARSData()
#'
#' @export

# rename cleanSarsData
cleanSARSData <- function() {
  sars_df = scrapeSARSData() %>%
    dplyr::rename(region = Country, 
                  cases = `Cumulative number of cases`,
                  deaths = Number_of_deaths,
                  recovered = `Number recovered`) %>%
    tidyr::gather(cases, deaths, recovered, key = "value_type", value = "value") %>%
    dplyr::mutate(disease = "sars") %>%
    dplyr::select(disease, region, date, value, value_type) %>%
    dplyr::mutate(value = as.integer(value)) %>%
    dplyr::arrange(date, region, value_type)
  # Cleaning region names
  sars_df$region[grepl("China Hong Kong Special Administrative Region", sars_df$region)] <- "Hong Kong Special Administrative Region of China"
  sars_df$region[grepl("United States", sars_df$region)] <- "United States"
  sars_df$region[grepl("Philippines", sars_df$region)] <- "Philippines"
  sars_df$region[grepl("Canada", sars_df$region)] <- "Canada"
  sars_df$region[grepl("Viet Nam", sars_df$region)] <- "Viet Nam"
  sars_df$region[sars_df$region %in% c("China Guangdong Province",
                                       "China3",
                                       "China^5")] <- "China"
  return(sars_df)
}

#' Import SARS data
#' 
#' Imports data from public github repo on SARS cases by date and location,
#'   reformats and cleans the data, and merges with population and latitude-longitude data
#'   
#' @param from_web defaults to FALSE: whether to import from the web or from the package
#' 
#' @return Output is a dataframe with columns for disease (sars),
#' region (country), value, value_type, pop_2016, lat (latitude), long (longitude)
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select rename filter
#' 
#' @examples
#' importSARSData()
#'
#' @export
#' 
importSARSData <- function(from_web = F) {
  if (!from_web) {
    data("sars_data", envir = environment())
    return (sars_data)
  }
  pop_map <- buildPopulationMap() %>%
    dplyr::select(sars_name, pop_2003)
  coord_map <- buildCoordinateMap() %>%
    dplyr::select(sars_name, latitude, longitude) %>%
    dplyr::rename("lat" = "latitude", "long" = "longitude")
  sars_data <- cleanSARSData() %>%
    dplyr::left_join(pop_map, by = c("region" = "sars_name")) %>%
    dplyr::left_join(coord_map, by = c("region" = "sars_name")) %>%
    dplyr::filter(!(region %in% c("Total", "Number of deaths")))
  sars_data$value[is.na(sars_data$value)] <- 0
  return (sars_data)
}

