% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/silasDataAugmentation.R
\name{buildCoordinateMap}
\alias{buildCoordinateMap}
\title{Get Map of Region Names to Latitude and Longitude}
\usage{
buildCoordinateMap()
}
\value{
Output is a dataframe with columns "region_name" 
  (the standard country name of region), "covid_name", "zika_name", "sars_name",
  "latitude", and "longitude"
}
\description{
Builds a dataset providing a map between the country names used in the Covid data,
  the Zika data, and the SARS data, and the latitude and longitude associated
  with each country
}
\examples{
buildCoordinateMap()

}
