#' Imports SARS disease data
#'
#' Imports the Kaggle SARS disease data from a public Github repository.
#' 
#' @return Output is a raw dataframe in its untidy format, directly from
#'  the Github repository 
#' @importFrom readr read_csv
#' 
#' @examples 
#' importSARSData()
#'
#' @export
importSARSData <- function() {
  link <- "https://raw.githubusercontent.com/mcolon21/ncov2019data/master/sars_data.csv"
  data <- readr::read_csv(link)
  return(data)
}


#' Tidy SARS disease data
#'
#' Tidies the imported SARS disease data to conform to the necessary tidy format.
#' 
#' @param raw_sars_df a raw, untidied dataframe of the format imported from the
#'  Gitbhub repository by the import function
#' 
#' @return Output is a dataframe with columns for date (Date), location (character),
#'   value_type (character; either "cases", "deaths", or "recoveries"), and value (int)
#'   
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr rename select arrange
#' 
#' @examples
#' data = importSARSData()
#' tidySARSData(data)
#'
tidySARSData <- function(raw_sars_df) {
  tidy_sars_df = raw_sars_df %>%
    dplyr::rename(location = Country, 
                  cases = `Cumulative number of cases`,
                  deaths = Number_of_deaths,
                  recoveries = `Number recovered`) %>%
    tidyr::gather(cases, deaths, recoveries, key = "value_type", value = "value") %>%
    dplyr::select(date, location, value_type, value) %>%
    dplyr::mutate(value = as.integer(value)) %>%
    dplyr::arrange(date, location, value_type)
  return(tidy_sars_data)
}