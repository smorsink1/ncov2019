
# ncov2019

# `ncov2019` Version 1.0.0
(latest update on 2020-03-23)

`ncov2019` is an R package for acquiring and visualizing data on COVID-19 and other diseases (Zika and SARS). It includes functions for scraping and cleaning data on each of these diseases, filtering disease data, and visualizing data on a map or a time-series graph. It also includes data: disease data by location for SARS, Zika, and COVID-19, country population data, and country latitude/longitude data.

ADD DISCLAIMERS about data reliability, 

## Functionality

The package functionality has three main phases: importing data, filtering data, and visualizing data.

To import cleaned disease data, use either `importCovidData()`, `importZikaData()`, or `importSARSData()`. Each of the import functions has a `from_web` argument. For Zika and SARS, getting the data from the web will give the same data as loading the data that is saved in the package (either `zika_data.rda` or `sars_data.rda`), so the default is `from_web = FALSE`. For COVID-19, one needs `from_web = TRUE` to get the most up-to-date information (this is the default behavior).

To filter disease data, use `filterDiseaseData()`. While other filtering methods, (such as `dplyr::filter()`, or using base R), sometimes yield the same results, using `filterDiseaseData()` is safer to ensure compatibility with the rest of the functions in the package.

To visualize disease data on a map, use `mapPlotStatic()` to generate a plot for a single date or `mapPlotAnimate()` to generate an animation across many dates. To visualize disease metrics over time, use the `plotTimeSeries()` function with specified arguments (see documentation). The behavior of `plotTimeSeries()` is largely determined by the `plot_what` argument, which can take on values like `"cases"`, `"cases_per_pop"`, `"log_cases"`, "`new_cases`", or `"growth_factor"`.

## Thanks to

Johns Hopkins University Center for Systems Science and Engineering for COVID-19 data, the CDC for Zika virus data, WHO for SARS data, Google for country coordinate data, and the World Bank for country population data. 

## More Information

For a deeper view of the functionality of ncov2019, check out the vignette.

