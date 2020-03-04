
#' Latitude and Longitude for countries
#' 
#' A dataset providing a map between the country names used in the Covid data,
#'   the Zika data, and the SARS data, and the latitude and longitude associated
#'   with each country
#'   
#' @name country_coordinates_map
#' @docType data
#'   
#' @format A data frame with 83 rows and 6 columns:
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
#' @format A .csv file with 265 rows and 5 columns
#' \describe{
#'   \item{country_name}
#'   \item{country_code}
#'   \item{pop_2003}
#'   \item{pop_2016}
#'   \item{pop_2018}
#' }
#' @source \url{https://data.worldbank.org/indicator/SP.POP.TOTL}
NULL


#' Zika Virus Data
#' 
#' Description
#'   
#' @format A data frame with 83 rows and 6 columns:
#' \describe{
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#' }
#' @source \url{url}
"data"

#' SARS Data
#' 
#' Description
#'   
#' @format A data frame with 83 rows and 6 columns:
#' \describe{
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#'   \item{col_name}{col_description}
#' }
#' @source \url{url}
"data"