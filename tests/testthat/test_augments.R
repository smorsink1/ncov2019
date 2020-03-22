context("Testing function for augmenting disease data")

test_that("dayOfDiseaseColumn works as expected", {
  sars <- dayOfDiseaseColumn(importSARSData(from_web = FALSE))
  threshold_rows <- sars %>%
    dplyr::filter(value_type == "cases") %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(total_cases = sum(value)) %>%
    dplyr::filter(total_cases >= 100)
  
  first_date <- min(threshold_rows$date)
  expect_true(all(which(sars$day_of_disease == 1) == which(sars$date == first_date)))
  
  covid <- dayOfDiseaseColumn(importCovidData(), threshold = 7000)
  threshold_rows <- covid %>%
    dplyr::filter(value_type == "cases") %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(total_cases = sum(value)) %>%
    dplyr::filter(total_cases >= 7000)
  
  seventh_date_of_threshold <- (min(threshold_rows$date) + 6)
  expect_true(all(which(covid$day_of_disease == 7) == which(covid$date == seventh_date_of_threshold)))
})


test_that("congregateDataDates works as expected", {
  zika_confirmed <- filterDiseaseData(importZikaData(), include_suspected = FALSE)
  zika_congregated <- congregateDataDates(zika_confirmed)
  prov <- "Bahia"
  country <- "Brazil"
  
  zika_confirmed_selected <- zika_confirmed %>%
    dplyr::filter(province == prov & region == country)
  
  zika_congregated_selected <- zika_congregated %>%
    dplyr::filter(province == prov & region == country)
  
  dts <- unique(zika_congregated$date)[order(unique(zika_congregated$date))]
  
  found_vec = c()
  for(d in dts) {
    congregated_value = zika_congregated_selected$value[zika_congregated_selected$date == d]
    found = FALSE
    if(congregated_value > 0) {
      rolling_d = (d - 6)
      while(!found & rolling_d <= d) {
        if(rolling_d %in% zika_confirmed_selected$date) {
          if(zika_confirmed_selected$value[zika_confirmed_selected$date == rolling_d] 
             == congregated_value)
            found = TRUE
        }
        rolling_d = rolling_d + 1
      }
      if(!found & congregated_value == 
         zika_congregated_selected$value[zika_congregated_selected$date == (d - 7)]) {
        found = TRUE
      }
    } else {
      data_error = FALSE
      rolling_d = (d - 6)
      while(!data_error & rolling_d <= d) {
        if(rolling_d %in% zika_confirmed_selected$date) {
          if(zika_confirmed_selected$value[zika_confirmed_selected$date == rolling_d] 
             > 0)
            data_error = TRUE
        }
        rolling_d = rolling_d + 1
      }
      found = !data_error
    }
    found_vec = c(found_vec, found)
  }
  
  expect_true(length(which(!found_vec)) == 0 & length(which(found_vec)) == length(dts))
})