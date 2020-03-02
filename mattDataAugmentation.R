


library("rvest")
library("magrittr")
url = "https://developers.google.com/public-data/docs/canonical/countries_csv"

coordinates = url %>%
  read_html() %>%
  html_nodes("table")

coord = html_table(coordinates[[1]])

View(left_join(sars, coord, by = c("location" = "name")))
