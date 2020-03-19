
context("Testing functions for importing disease data")

test_that("importCovidData works as expected", {
  covid_data <- importCovidData(from_web = T)
  expect_true("disease" %in% names(covid_data))
  expect_true("province" %in% names(covid_data))
  expect_true("region" %in% names(covid_data))
  expect_true("lat" %in% names(covid_data))
  expect_true("long" %in% names(covid_data))
  expect_true("date" %in% names(covid_data))
  expect_true("value" %in% names(covid_data))
  expect_true("value_type" %in% names(covid_data))
  expect_true("pop_2018" %in% names(covid_data))
  expect_true(all(covid_data$disease == "covid"))
  expect_true("cases" %in% unique(covid_data$value_type))
  expect_true("deaths" %in% unique(covid_data$value_type))
  expect_true("recovered" %in% unique(covid_data$value_type))
})

test_that("importZikaData works as expected", {
  zika_data <- importZikaData(from_web = T)
  expect_true("disease" %in% names(zika_data))
  expect_true("province" %in% names(zika_data))
  expect_true("region" %in% names(zika_data))
  expect_true("lat" %in% names(zika_data))
  expect_true("long" %in% names(zika_data))
  expect_true("date" %in% names(zika_data))
  expect_true("value" %in% names(zika_data))
  expect_true("value_type" %in% names(zika_data))
  expect_true("pop_2016" %in% names(zika_data))
  expect_true(all(zika_data$disease == "zika"))
  expect_true("cumulative_confirmed_cases" %in% unique(zika_data$value_type))
  expect_true("cumulative_suspected_cases" %in% unique(zika_data$value_type))
})

test_that("importSARSData works as expected", {
  sars_data <- importSARSData(from_web = T)
  expect_true("disease" %in% names(sars_data))
  expect_true("region" %in% names(sars_data))
  expect_true("lat" %in% names(sars_data))
  expect_true("long" %in% names(sars_data))
  expect_true("date" %in% names(sars_data))
  expect_true("value" %in% names(sars_data))
  expect_true("value_type" %in% names(sars_data))
  expect_true("pop_2003" %in% names(sars_data))
  expect_true(all(sars_data$disease == "sars"))
  expect_true("cases" %in% unique(sars_data$value_type))
  expect_true("deaths" %in% unique(sars_data$value_type))
  expect_true("recovered" %in% unique(sars_data$value_type))
})

## Note that these tests implicitly test the DataAugmentation scripts as well


