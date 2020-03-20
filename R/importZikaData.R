
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

#' Reformat Zika Data
#' 
#' Imports data from public github repo on Zika cases by date and location, 
#'   then reformats the data into date/location/values type combinations
#' 
#' @return Output is a dataframe with columns for province, region, date, value, and value_type
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename filter
#' 
#' @examples 
#' reformatZikaData()
#' 
#' @export
#' 
reformatZikaData <- function() {
  data <- scrapeZikaData()
  data$disease <- "zika"
  # province and location
  data$region <- data$location %>%
    strsplit(split = "-") %>%
    sapply(FUN = `[`, 1)
  data$province <- data$location %>%
    strsplit(split = "-") %>%
    sapply(FUN = `[`, 2)
  data_tidy <- data %>%
    # Brazil subregions (which have cumulative stats) are coded as "region"
    # since they simply accumulate data that is already present, these rows are removed
    dplyr::filter(!(region %in% c("Centro", "Nordeste", "Norte", "Sudeste", "Sul"))) %>%
    # drop unnecessary columns
    dplyr::select(-data_field_code, -time_period, -time_period_type, -unit,
                  -location, -location_type) %>%
    # renaming and reordering to match consistent format
    dplyr::rename("value_type" = "data_field", "date" = "report_date") %>%
    # dropping non-zika reports 
    dplyr::filter(!grepl("microcephaly", value_type)) %>%
    dplyr::select(disease, province, region, date, value, value_type)
  return (data_tidy)
}

#' Clean Zika Data
#' 
#' Imports data from public github repo on Zika cases by date and location, 
#'   reformats the data into date/location/values type combinations, and cleans
#'   the values_type column to give proxies for confirmed cases for each row
#' 
#' @return Output is a dataframe with columns for province, region, date, value, and value_type
#' 
#' @importFrom dplyr filter group_by summarize mutate bind_rows recode
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' 
#' @examples 
#' cleanZikaData()
#' 
#' @export
#'
cleanZikaData <- function() {
  data <- reformatZikaData()
  data_split <- split(data, data$region)
  # Argentina
  data_split[["Argentina"]] <- data_split[["Argentina"]] %>%
    dplyr::filter(value_type %in% c("cumulative_confirmed_imported_cases", 
                                    "cumulative_confirmed_local_cases")) %>%
    dplyr::group_by(disease, province, region, date) %>%
    dplyr::summarize(value = sum(value, na.rm = T)) %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases")
  # Brazil
  data_split[["Brazil"]] <- data_split[["Brazil"]] %>%
    dplyr::filter(value_type == "zika_reported") %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases") %>%
    dplyr::filter(!is.na(province))  # NA rows are cumulative rows
  # Colombia (significant problems with reporting)
  data_split[["Colombia"]] <- data_split[["Colombia"]] %>%
    tidyr::pivot_wider(names_from = value_type, values_from = value, values_fn = list(value = sum)) %>%
    dplyr::group_by(disease, province, region, date) %>%
    dplyr::summarize("value" = sum(zika_confirmed_laboratory, na.rm = T) + sum(zika_confirmed_clinic, na.rm = T)) %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases")
  # Dominican_Republic
  data_split[["Dominican_Republic"]] <- data_split[["Dominican_Republic"]] %>%
    dplyr::filter(grepl("zika", value_type)) %>%
    dplyr::filter(value_type == "zika_confirmed_pcr_cumulative") %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases") %>%
    ## the rows with NA for province are totals across DR, numbers check out, so
    dplyr::filter(!is.na(province))
  # Ecuador
  data_split[["Ecuador"]] <- data_split[["Ecuador"]] %>%
    dplyr::filter(value_type == "total_zika_confirmed_autochthonous") %>%
    dplyr::group_by(disease, province, region, date) %>% 
    dplyr::summarize(value = sum(value)) %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases")
  # El_Salvador
  data_split[["El_Salvador"]] <- data_split[["El_Salvador"]] %>%
    dplyr::filter(value_type %in% c("cumulative_suspected_total", "cumulative_confirmed")) %>%
    dplyr::mutate(value_type = dplyr::recode(value_type, 
                                      "cumulative_suspected_total" = "cumulative_suspected_cases",
                                      "cumulative_confirmed" = "cumulative_confirmed_cases")) %>%
    ## the rows with NA for province are totals across region, numbers check out, so
    dplyr::filter(!is.na(province))
  # Guatemala
  data_split[["Guatemala"]] <- data_split[["Guatemala"]] %>%
    dplyr::filter(value_type == "total_zika_confirmed_cumulative") %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases") %>%
    ## the rows with NA for province are totals across region, numbers check out, so
    dplyr::filter(!is.na(province))
  # Haiti
  data_split[["Haiti"]] <- data_split[["Haiti"]] %>%
    dplyr::filter(value_type == "total_zika_new_suspected_cumulative") %>%
    dplyr::mutate(value_type = "cumulative_suspected_cases") %>%
    dplyr::filter(!is.na(province)) # NA row is a cumulative row
  # Mexico
  data_split[["Mexico"]] <- data_split[["Mexico"]] %>%
    dplyr::filter(value_type == "weekly_zika_confirmed") %>%
    dplyr::group_by(disease, province, region) %>%
    dplyr::mutate(value = cumsum(value)) %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases")
  # Nicaragua
  data_split[["Nicaragua"]] <- data_split[["Nicaragua"]] %>%
    dplyr::filter(value_type == "total_zika_confirmed_cumulative") %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases") %>%
    ## most data has NA for province (assume it's country-wide)
    dplyr::mutate(province = "Nicaragua")
  # Panama
  data_split[["Panama"]] <- data_split[["Panama"]] %>%
    dplyr::filter(grepl("Zika_confirmed_laboratory", value_type)) %>%
    dplyr::group_by(disease, province, region, date) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases")
  # Puerto_Rico
  data_split[["Puerto_Rico"]] <- data_split[["Puerto_Rico"]] %>%
    dplyr::filter(value_type == "zika_confirmed_cumulative_2015-2016") %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases") %>%
    ## all data has NA for province (assume it's country-wide)
    dplyr::mutate(province = "Puerto_Rico")
  # United_States
  data_split[["United_States"]] <- data_split[["United_States"]] %>%
    dplyr::group_by(disease, province, region, date) %>%
    dplyr::summarize(value = sum(value))  %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases")
  # United_States_Virgin_Islands
  data_split[["United_States_Virgin_Islands"]] <- data_split[["United_States_Virgin_Islands"]] %>%
    dplyr::filter(value_type == "zika_reported") %>%
    dplyr::mutate(value_type = "cumulative_confirmed_cases") %>%
    dplyr::filter(!is.na(province))  # NA row is cumulative
  return (dplyr::bind_rows(data_split))
}

#' Import Zika Data
#' 
#' Imports data from public github repo on Zika cases by date and location,
#'   reformats, cleans, and merges with population and latitude-longitude data
#'
#' @param from_web defaults to FALSE: whether to import from the web or from the package
#' 
#' @return Output is a dataframe with columns for disease (zika), province (location specific), 
#' region (location general), value, value_type, pop_2016, lat (latitude), long (longitude)
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select rename
#' 
#' @examples 
#' importZikaData()   # from_web defaults to FALSE
#' importZikaData(from_web = T) 
#' 
#' @export
#' 
importZikaData <- function(from_web = F) {
  if (!from_web) {
    data("zika_data", envir = environment())
    return (zika_data)
  }
  pop_map <- buildPopulationMap() %>%
    dplyr::select(zika_name, pop_2016)
  coord_map <- buildCoordinateMap() %>%
    dplyr::select(zika_name, latitude, longitude) %>%
    dplyr::rename("lat" = "latitude", "long" = "longitude")
  zika_data <- cleanZikaData() %>%
    dplyr::left_join(pop_map, by = c("region" = "zika_name")) %>%
    dplyr::left_join(coord_map, by = c("region" = "zika_name"))
  return (zika_data)
}
