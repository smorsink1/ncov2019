
#' Get Map of Region Names to Latitude and Longitude
#'
#' Builds a dataset providing a map between the country names used in the Covid data,
#'   the Zika data, and the SARS data, and the latitude and longitude associated
#'   with each country
#'   
#' @return Output is a dataframe with columns "region_name" 
#'   (the standard country name of region), "covid_name", "zika_name", "sars_name",
#'   "latitude", and "longitude"
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr left_join full_join filter
#' @importFrom tidyselect everything
#'
#' @export
#'   
buildCoordinateMap <- function() {
  covid_data <- importCovidData()
  zika_data <- importZikaData()
  sars_data <- cleanSARSData()
  country_data <- importCoordinateData()
  
  # Covid
  covid_countries <- unique(covid_data$region)
  covid_joined <- dplyr::left_join(tibble::tibble("region" = covid_countries), country_data, by = "region")
  covid_map <- tibble::tibble("covid_name" = covid_countries, "region_name" = covid_joined$region)
  covid_map$region_name[is.na(covid_joined$latitude)] <- NA
  covid_map$region_name[covid_map$covid_name == "Mainland China"] <- "China"
  covid_map$region_name[covid_map$covid_name == "US"] <- "United States"
  covid_map$region_name[covid_map$covid_name == "UK"] <- "United Kingdom"
  covid_map$region_name[covid_map$covid_name == "North Macedonia"] <- "Macedonia [FYROM]"
  
  # Zika
  zika_countries <- unique(zika_data$region)
  zika_joined <- dplyr::left_join(tibble::tibble("region" = zika_countries), country_data, by = "region")
  zika_map <- tibble::tibble("zika_name" = zika_countries, "region_name" = zika_joined$region)
  zika_map$region_name[is.na(zika_joined$latitude)] <- NA
  zika_map$region_name[zika_map$zika_name == "Dominican_Republic"] <- "Dominican Republic"
  zika_map$region_name[zika_map$zika_name == "El_Salvador"] <- "El Salvador"
  zika_map$region_name[zika_map$zika_name == "Puerto_Rico"] <- "Puerto Rico"
  zika_map$region_name[zika_map$zika_name == "United_States"] <- "United States"
  zika_map$region_name[zika_map$zika_name == "United_States_Virgin_Islands"] <- "U.S. Virgin Islands"
  
  # SARS
  sars_countries <- unique(sars_data$region)
  sars_joined <- dplyr::left_join(tibble::tibble("region" = sars_countries), country_data, by = "region")
  sars_map <- tibble::tibble("sars_name" = sars_countries, "region_name" = sars_joined$region)
  sars_map$region_name[is.na(sars_joined$latitude)] <- NA
  sars_map$region_name[sars_map$sars_name == "Hong Kong Special Administrative Region of China"] <- "Hong Kong"
  sars_map$region_name[sars_map$sars_name == "Viet Nam"] <- "Vietnam"
  sars_map$region_name[sars_map$sars_name == "Taiwan China"] <- "Taiwan"
  sars_map$region_name[sars_map$sars_name == "China Taiwan"] <- "Taiwan"
  sars_map$region_name[sars_map$sars_name == "Republic of Ireland"] <- "Ireland"
  sars_map$region_name[sars_map$sars_name == "Republic of Korea"] <- "South Korea"
  sars_map$region_name[sars_map$sars_name == "China Macao Special Administrative Region"] <- "Macau"
  sars_map$region_name[sars_map$sars_name == "Russian Federation"] <- "Russia"
  
  region_map <- covid_map %>%
    dplyr::full_join(zika_map, by = "region_name") %>%
    dplyr::full_join(sars_map, by = "region_name") %>%
    dplyr::filter(!is.na(region_name)) %>%
    dplyr::left_join(country_data, by = c("region_name" = "region")) %>%
    dplyr::select(region_name, tidyselect::everything())
  
  return (region_map)
}

#' Get Map of Region Names to Population
#'
#' Builds a dataset providing a map between the country names used in the Covid data,
#'   the Zika data, and the SARS data, and the population associated with each country
#'   
#' @return Output is a dataframe with columns "region_name" 
#'   (the standard country name of region), "covid_name", "zika_name", "sars_name",
#'   "latitude", and "longitude"
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr left_join full_join filter
#' @importFrom tidyselect everything
#'
#' @examples 
#'   data(pop_data_raw)
#'   buildPopulationMap(pop_data_raw)
#' 
#' @export
#'   
buildPopulationMap <- function(pop_data) {
  # TODO: change these names from import to something else
  covid_data <- importCovidData()
  zika_data <- importZikaData()
  sars_data <- cleanSARSData()
  
  # Covid
  covid_countries <- unique(covid_data$region)
  covid_joined <- dplyr::left_join(tibble::tibble("region" = covid_countries), 
                                   pop_data, by = c("region" = "country_name"))
  covid_map <- tibble::tibble("covid_name" = covid_countries, "region_name" = covid_joined$region)
  covid_map$region_name[is.na(covid_joined$country_code)] <- NA
  covid_map$region_name[covid_map$covid_name == "Mainland China"] <- "China"
  covid_map$region_name[covid_map$covid_name == "South Korea"] <- "Korea, Rep."
  covid_map$region_name[covid_map$covid_name == "Taiwan"] <- "EEEEEEEEEEEEEEEK"
  #### no Taiwan in the population data
  covid_map$region_name[covid_map$covid_name == "US"] <- "United States"
  covid_map$region_name[covid_map$covid_name == "Macau"] <- "Macao SAR, China"
  covid_map$region_name[covid_map$covid_name == "Hong Kong"] <- "Hong Kong SAR, China"
  covid_map$region_name[covid_map$covid_name == "UK"] <- "United Kingdom"
  covid_map$region_name[covid_map$covid_name == "Russia"] <- "Russian Federation"
  covid_map$region_name[covid_map$covid_name == "Egypt"] <- "Egypt, Arab Rep."
  covid_map$region_name[covid_map$covid_name == "Iran"] <- "Iran, Islamic Rep."
  
  # Zika
  zika_countries <- unique(zika_data$region)
  zika_joined <- dplyr::left_join(tibble::tibble("region" = zika_countries), 
                                  pop_data, by = c("region" = "country_name"))
  zika_map <- tibble::tibble("zika_name" = zika_countries, "region_name" = zika_joined$region)
  zika_map$region_name[is.na(zika_joined$country_code)] <- NA
  zika_map$region_name[zika_map$zika_name == "Dominican_Republic"] <- "Dominican Republic"
  zika_map$region_name[zika_map$zika_name == "El_Salvador"] <- "El Salvador"
  zika_map$region_name[zika_map$zika_name == "Puerto_Rico"] <- "Puerto Rico"
  zika_map$region_name[zika_map$zika_name == "United_States"] <- "United States"
  zika_map$region_name[zika_map$zika_name == "United_States_Virgin_Islands"] <- "Virgin Islands (U.S.)"
  
  # SARS
  sars_countries <- unique(sars_data$region)
  sars_joined <- dplyr::left_join(tibble::tibble("region" = sars_countries), 
                                  pop_data, by = c("region" = "country_name"))
  sars_map <- tibble::tibble("sars_name" = sars_countries, "region_name" = sars_joined$region)
  sars_map$region_name[is.na(sars_joined$country_code)] <- NA
  sars_map$region_name[sars_map$sars_name == "Hong Kong Special Administrative Region of China"] <- "Hong Kong SAR, China"
  sars_map$region_name[sars_map$sars_name == "Viet Nam"] <- "Vietnam"
  sars_map$region_name[sars_map$sars_name == "Taiwan China"] <- "EEEEEEEEEEEEEEEK"
  sars_map$region_name[sars_map$sars_name == "China Taiwan"] <- "EEEEEEEEEEEEEEEK"
  sars_map$region_name[sars_map$sars_name == "Republic of Ireland"] <- "Ireland"
  sars_map$region_name[sars_map$sars_name == "Republic of Korea"] <- "Korea, Rep."
  sars_map$region_name[sars_map$sars_name == "China Macao Special Administrative Region"] <- "Macao SAR, China"
  
  region_map <- covid_map %>%
    dplyr::full_join(zika_map, by = "region_name") %>%
    dplyr::full_join(sars_map, by = "region_name") %>%
    dplyr::filter(!is.na(region_name)) %>%
    dplyr::left_join(pop_data, by = c("region_name" = "country_name")) %>%
    dplyr::select(region_name, tidyselect::everything())
  
  return (region_map)
}

