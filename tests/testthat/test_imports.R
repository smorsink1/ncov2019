
context("Testing functions for importing disease data")

test_that("importCovidData works as expected", {
  covid_data <- importCovidData(from_web = T)
  expect_true("value" %in% names(covid_data))
})

test_that("importZikaData works as expected", {
  
})






