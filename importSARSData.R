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
scrapeSARSData <- function() {
  link <- "https://raw.githubusercontent.com/mcolon21/ncov2019data/master/sars_data.csv"
  data <- suppressMessages(readr::read_csv(link))
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
importSARSData <- function() {
  sars_df = scrapeSARSData() %>%
    dplyr::rename(region = Country, 
                  cases = `Cumulative number of cases`,
                  deaths = Number_of_deaths,
                  recoveries = `Number recovered`) %>%
    tidyr::gather(cases, deaths, recoveries, key = "value_type", value = "value") %>%
    dplyr::mutate(disease = "sars") %>%
    dplyr::select(disease, date, region, value_type, value) %>%
    dplyr::mutate(value = as.integer(value)) %>%
    dplyr::arrange(date, region, value_type)
  return(sars_df)
}