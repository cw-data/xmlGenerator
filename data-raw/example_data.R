## code to prepare `example_data` dataset goes here

library(readxl)
example_data <- readxl::read_excel("data-raw/testdata.xlsx")
usethis::use_data(example_data, overwrite = TRUE)
