% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{covid_data}
\alias{covid_data}
\title{Coronavirus Data}
\format{A tibble with 27405 rows and 9 columns
\describe{
  \item{disease}{"covid" for all rows}
  \item{province}{the most granular location identifier}
  \item{region}{a general location identifier, usually the country}
  \item{lat}{the latitude of the location}
  \item{long}{the longitude of the location}
  \item{date}{the date of the observation}
  \item{value}{an integer representing the quantity of value_type for that observation}
  \item{value_type}{either "cases", "deaths", or "recovered"}
  \item{pop_2018}{region population data for 2018}
}}
\source{
\url{https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv}
}
\description{
A dataset of coronavirus data with rows being location/date/value_type combinations, 
  columns being identifying information and the associated value, updated thru 2020-03-06
}
