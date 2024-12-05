## code to prepare `indices` dataset goes here

indices <- read.csv(file = "data-raw/indices.csv")
usethis::use_data(indices, overwrite = TRUE)
