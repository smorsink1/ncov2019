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


#' Imports SARS disease data
#'
#' Tidies and imports the SARS disease data to conform to the necessary tidy format.
#' 
#' @return Output is a dataframe with columns for date (Date), region (character),
#'   value_type (character; either "cases", "deaths", or "recoveries"), and value (int)
#'   
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr rename select arrange
#' 
#' @examples
#' importSARSData()
#'
#' @export

# rename cleanSarsData
importSARSData <- function() {
  sars_df = scrapeSARSData() %>%
    dplyr::rename(region = Country, 
                  cases = `Cumulative number of cases`,
                  deaths = Number_of_deaths,
                  recoveries = `Number recovered`) %>%
    tidyr::gather(cases, deaths, recoveries, key = "value_type", value = "value") %>%
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

# write importSarsData
# which involves merging with the coordinate and population data