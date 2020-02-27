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

# parsing issues:
## * in value in Brazil entries (rows 2415, 2784, 5193)
## 2415 is: 125*5
## 2784 is: 149*5
## 5193 is: 5*
## underscores in Puerto Rico date formats (rows 104408, 104409, ..., )
### mostly it's 2016_03_30 format, some of it is 2016_04-06 format

## this covers basically all parsing failures 
## (Puerto Rico takes up ~ 250 rows of bad dates)

# solution is probably to parse report_date as character,
#                         parse value as character
# fix after importing
