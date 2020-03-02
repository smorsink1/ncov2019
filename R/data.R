
#' Latitude and Longitude for countries
#' 
#' A dataset providing a map between the country names used in the Covid data,
#'   the Zika data, and the SARS data, and the latitude and longitude associated
#'   with each country
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
"country_coordinates_map"