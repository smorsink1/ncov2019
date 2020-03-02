
#' Downloads Zika virus data
#'
#' Imports the Kaggle Zika virus data from a public Github repository.
#' 
#' @return Output is a dataframe with rows for date-location-case_type pairs
#'   
#' @importFrom readr read_csv cols col_character
#' @importFrom magrittr %>%
#' 
#' @examples
#' scrapeZikaData()
#' 
#' @export
#'
scrapeZikaData <- function() {
  link <- "https://raw.githubusercontent.com/mcolon21/ncov2019data/master/cdc_zika.csv"
  data <- tryCatch(readr::read_csv(link, col_types = readr::cols(value = readr::col_character(),
                                                                 report_date = readr::col_character())),
                   error = function(e) stop ("Data no longer found at url"))
  # parsing issues:
  ## * in value in Brazil entries (rows 2415, 2784, 5193)
  ### 2415 is: 125*5 (should be 125), 2784 is: 149*5 (should be 149), 5193 is: 5* (should be 5)
  ## underscores in Puerto Rico date formats (rows 104408, 104409, ..., )
  ### mostly it is 2016_03_30 format, some of it is 2016_04-06 format
  removeAfterChar <- function(string, char) {
    # given a string and a character, 
    # removes the everything from the first appearance of specified character onwards
    string %>%
      strsplit(char, fixed = T) %>%
      `[[`(1) %>%
      `[`(1)
  }
  data$value[2415] <- removeAfterChar(data$value[2415], "*")
  data$value[2784] <- removeAfterChar(data$value[2784], "*")
  data$value[5193] <- removeAfterChar(data$value[5193], "*")
  data$value <- as.integer(data$value)
  report_date_dashed <- gsub(pattern = "_", replacement = "-", x = data$report_date)
  data$report_date <- as.Date(report_date_dashed)
  return (data)
}

#' Imports Tidy Zika Data
#' 
#' Imports data from public github repo on Zika cases by date and location, 
#'   then collects all information into a tidy dataset
#' 
#' @return Output is a dataframe with columns for province, region, date, value, and value_type
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename
#' 
#' @examples 
#' importZikaData(scrapeZikaData())
#' 
#' @export
#' 
importZikaData <- function() {
  data <- scrapeZikaData()
  data$disease <- "zika"
  data$region <- data$location %>%
    strsplit(split = "-") %>%
    sapply(FUN = `[`, 1)
  data$province <- data$location %>%
    strsplit(split = "-") %>%
    sapply(FUN = `[`, 2)
  data_tidy <- data %>%
    # drop unnecessary columns
    dplyr::select(-data_field_code, -time_period, -time_period_type, -unit,
                  -location, -location_type) %>%
    # renaming and reordering to match consistent format
    dplyr::rename("value_type" = "data_field", "date" = "report_date") %>%
    dplyr::select(disease, province, region, date, value, value_type)
  return (data_tidy)
}
