library(testthat)

context("testing the dimensions of a country run from package database with fpemmodeling::do_1country_run")
test_that("test one country dimensions", {
  code <- 288#ghana#586 pakistan
  first_year <- 1990
  last_year <- 2030
  is_in_union <- "Y"
  res <- fpemmodeling::do_1country_run(surveydata_filepath = NULL,
                                         division_numeric_code = code,
                                         is_in_union = is_in_union,
                                         first_year = first_year,
                                         last_year = last_year
  )
  resdim <- res$posterior_samples %>% dim
  expecteddim <- c(1, 4500, 42, 3)
  expect_true(all(resdim == expecteddim))
})

