#' Imports SARS disease data
#'
#' Imports the Kaggle SARS disease data from a public Github repository.
#' 
#' @param 
#' @param 
#' 
#' @export 
#' 
#' # import from packages: 
#' @importFrom readr read_csv
#' 
#' # Examples like this: <at>examples
#'
#'
importSARSData <- function() {
  link <- "https://raw.githubusercontent.com/mcolon21/ncov2019data/master/sars_data.csv"
  data <- readr::read_csv(link)
}