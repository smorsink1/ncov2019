% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mattDataViz.R
\name{mapPlotStatic}
\alias{mapPlotStatic}
\title{Static Map Plot}
\usage{
mapPlotStatic(data, selected_date = NA, selected_value_type = NA,
  color = "red", alpha = 0.5, congregate = FALSE)
}
\arguments{
\item{data}{The data frame that contains the data to be plotted on the world map.
This data frame must contain the following columns: "long" (type "numeric"), 
"lat" (type "numeric"), and "value" (type "numeric").}

\item{selected_date}{If the data frame contains a column "date" with multiple dates,
this parameter must specify a specific date to be plotted.}

\item{selected_value_type}{If the data frame contains a column "value_type" with multiple dates,
this parameter must specify a specific data value type to be plotted. Options are "cases", "deaths", 
and "recovered".}

\item{color}{Color of points on world map. Default is red.}

\item{alpha}{Transparency level of points on world map. Default is 0.5.}

\item{congregate}{Only applies to Zika data. Since Zika data was tracked on a scattered weekly basis,
this determines whether or not the user would like to congregate the data (TRUE) or just to
see the data tracked on the selected date in the passed-in data frame (FALSE). Note that if congregate is
TRUE, the exact date won't be returned, but rather the closest congregation date prior to the selected date.
Defaults to FALSE.}
}
\value{
Output is the ggplot object which maps the chosen value on a world map using
latitude and longitude data.
}
\description{
Generates a static global map plot of a certain value of data.
}
\examples{
covid_data <- importCovidData(from_web = FALSE)
sars_data <- importSARSData()

mapPlotStatic(covid_data, selected_date = "2020-03-01", 
selected_value_type = "cases", alpha = 0.3)
mapPlotStatic(sars_data, selected_date = "2003-04-02", 
selected_value_type = "deaths", color = "purple")

}
