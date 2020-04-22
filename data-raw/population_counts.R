population_counts <- readr::read_csv("data-raw/population_counts.csv") #use of readdelimn here is to format as tibble
usethis::use_data(population_counts, overwrite = TRUE)
