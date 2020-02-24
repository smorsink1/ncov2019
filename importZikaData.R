#' Imports Zika virus data
#'
#' Imports the Kaggle Zika virus data from a public Github repository.
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
importZikaData <- function() {
  link <- "https://raw.githubusercontent.com/mcolon21/ncov2019data/master/cdc_zika.csv"
  data <- readr::read_csv(link)
}
