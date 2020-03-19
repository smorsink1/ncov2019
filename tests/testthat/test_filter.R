context("Testing function for filtering disease data")

test_that("filterDiseaseData works as expected", {
  zika <- importZikaData(from_web = F)
  
  zika_confirmed <- filterDiseaseData(zika)
  expect_true(nrow(zika[which(zika$value_type == "cumulative_confirmed_cases"), ]) == nrow(zika_confirmed))
  
  zika_all <- filterDiseaseData(zika, include_suspected = TRUE)
  expect_true(nrow(zika) == nrow(zika_all))
  
  zika_dates <- filterDiseaseData(zika, first_date = min(zika$date), last_date = max(zika$date))
  expect_true(nrow(zika) == nrow(zika_dates))
  
  zika_specific_date <- filterDiseaseData(zika, first_date = unique(zika$date)[15], last_date = unique(zika$date)[15])
  expect_true(length(unique(zika_specific_date$date)) == 1 & 
                unique(zika_specific_date$date)[1] == unique(zika$date)[15])
  
  zika_up_to_date <- filterDiseaseData(zika, last_date = unique(zika$date)[order(unique(zika$date))][28])
  expect_true(length(unique(zika_up_to_date$date)) > 1 & 
                min(zika_up_to_date$date) == min(zika$date) &
                max(zika_up_to_date$date) == unique(zika$date)[order(unique(zika$date))][28])
  
  zika_from_on_date <- filterDiseaseData(zika, first_date = unique(zika$date)[order(unique(zika$date))][28])
  expect_true(length(unique(zika_from_on_date$date)) > 1 & 
                max(zika_from_on_date$date) == max(zika$date) &
              min(zika_from_on_date$date) == unique(zika$date)[order(unique(zika$date))][28])
  
  
  zika_countries <- filterDiseaseData(zika, country = c("Brazil", "Mexico"))
  
  expect_true(length(unique(zika_countries$region)) == 2 &
                "Brazil" %in% zika_countries$region & 
                "Mexico" %in% zika_countries$region)
  
  zika_province <- filterDiseaseData(zika, country = c("Brazil", "Mexico", "Argentina"), 
                                     province = c("CABA"))
  expect_true(length(unique(zika_province$province)) == 1 &
                "CABA" %in% zika_province$province & 
                length(unique(zika_province$region)) == 1 & 
                "Argentina" %in% zika_province$region)
  
  zika_min_and_max <- filterDiseaseData(zika, min_value = 17, max_value = 39)
  
  expect_true(min(zika_min_and_max) >= 17 & 
                max(zika_min_and_max) <= 39)
  
  expect_error(filterDiseaseData(zika, first_date = unique(zika$date)[order(unique(zika$date))][35],
                                 last_date = unique(zika$date)[order(unique(zika$date))][32]))
  expect_error(filterDiseaseData(zika, min_value = 5, max_value = 3))
  expect_error(filterDiseaseData(zika, min_value = -18))
               
  
  sars <- importSARSData(from_web = F)
  sars_all <- filterDiseaseData(sars, first_date = "2003-05-01", last_date = "2002-05-31")
  expect_true(length(unique(sars_all$value_type)) == 3 & 
                "cases" %in% sars_all$value_type & 
                "deaths" %in% sars_all$value_type & 
                "recovered" %in% sars_all$value_type)
  
  sars_specific_value_type = filterDiseaseData(sars, type = "recovered")
  expect_true(length(unique(sars_all$value_type)) == 1 & 
                "recovered" %in% sars_all$value_type)
})



