context("Testing function for augmenting disease data")

test_that("dayOfDiseaseColumn works as expected", {
  sars <- dayOfDiseaseColumn(importSARSData(from_web = F))
  threshold_rows <- sars[which(sars$value_type == "cases" & sars$value >= 100), ]
  first_date <- min(threshold_rows$date)
  true_vec <- which(sars$day_of_disease == 1) == which(sars$date == first_date)
  expect_true(length(unique(true_vec)) == 1 & unique(true_vec)[1])
  
  covid <- dayOfDiseaseColumn(importCovidData(), threshold = 7)
  threshold_rows <- covid[which(covid$value_type == "cases" & covid$value >= 7), ]
  seventh_date <- (min(threshold_rows$date) + 6)
  true_vec <- which(covid$day_of_disease == 7) == which(covid$date == seventh_date)
  expect_true(length(unique(true_vec)) == 1 & unique(true_vec)[1])
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