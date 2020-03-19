
#' Latitude and Longitude for countries
#' 
#' A dataset providing a map between the country names used in the Covid data,
#'   the Zika data, and the SARS data, and the latitude and longitude associated
#'   with each country
#'   
#' @name country_coordinates_map
#' @docType data
#'   
#' @format A tibble with 83 rows and 6 columns:
#' \describe{
#'   \item{country_name}{standard name of the country}
#'   \item{covid_name}{name of the country in the Covid data}
#'   \item{zika_name}{name of the country in the Zika data}
#'   \item{sars_name}{name of the country in the SARS data}
#'   \item{latitude}{country location in latitude}
#'   \item{longitude}{country location in longitude}
#' }
#' @source \url{https://developers.google.com/public-data/docs/canonical/countries_csv}
NULL

#' Population Data for Countries
#'
#' A dataset with rows for countries (or groups of countries), columns for years in 
#'   which the diseases occurred (2003 for SARS, 2016 for Zika, 
#'   2018 for Covid (most recent year of data)), values are population
#'   
#' @name country_pop_data_raw
#' @docType data
#' 
#' @format A tibble with 265 rows and 5 columns
#' \describe{
#'   \item{country_name}
#'   \item{country_code}
#'   \item{pop_2003}
#'   \item{pop_2016}
#'   \item{pop_2018}
#' }
#' @source \url{https://data.worldbank.org/indicator/SP.POP.TOTL}
NULL


#' Coronavirus Data
#'
#' A dataset of coronavirus data with rows being location/date/value_type combinations, 
#'   columns being identifying information and the associated value, updated thru 2020-03-06
#'   
#' @name covid_data
#' @docType data
#' 
#' @format A tibble with 27405 rows and 9 columns
#' \describe{
#'   \item{disease} "covid" for all rows
#'   \item{province} the most granular location identifier
#'   \item{region} a general location identifier, usually the country
#'   \item{lat} the latitude of the location
#'   \item{long} the longitude of the location
#'   \item{date} the date of the observation
#'   \item{value} an integer representing the quantity of value_type for that observation
#'   \item{value_type} either "cases", "deaths", or "recovered"
#'   \item{pop_2018} region population data for 2018
#' }
#' @source \url{https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv}
NULL

#' Zika Data
#'
#' A dataset of Zika virus data with rows being location/date/value_type combinations, 
#'   columns being identifying information and the associated value
#'   
#' @name zika_data
#' @docType data
#' 
#' @format A tibble with 4228 rows and 9 columns
#' \describe{
#'   \item{disease} "zika" for all rows
#'   \item{province} the most granular location identifier
#'   \item{region} a general location identifier, usually the country
#'   \item{date} the date of the observation
#'   \item{value} an integer representing the quantity of value_type for that observation
#'   \item{value_type} either "cumulative_confirmed_cases" or "cumulative_suspected_cases"
#'   \item{pop_2016} region population data for 2016
#'   \item{lat} the latitude of the region
#'   \item{long} the longitude of the region
#' }
#' @source \url{https://raw.githubusercontent.com/mcolon21/ncov2019data/master/cdc_zika.csv}
NULL

#' SARS Data
#'
#' A dataset of SARS virus data with rows being location/date/value_type combinations, 
#'   columns being identifying information and the associated value
#'   
#' @name sars_data
#' @docType data
#' 
#' @format A tibble with 31551 rows and 8 columns
#' \describe{
#'   \item{disease} "sars" for all rows
#'   \item{region} a general location identifier, usually the country
#'   \item{date} the date of the observation
#'   \item{value} an integer representing the quantity of value_type for that observation
#'   \item{value_type} either "cases", "deaths", or "recovered"
#'   \item{pop_2003} region population data for 2003
#'   \item{lat} the latitude of the region
#'   \item{long} the longitude of the region
#' }
#' @source \url{https://raw.githubusercontent.com/mcolon21/ncov2019data/master/sars_data.csv}
NULL

