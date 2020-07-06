library(testthat)

context("testing the dimensions of a country run from package database with fpemmodeling::do_1country_run")
test_that("test a country to see if we get an error", {
  code <- 288#ghana#586 pakistan
  first_year <- 1990
  last_year <- 2030
  is_in_union <- "Y"
  res <- FPEMcountry::fit_fp_c(
                                         division_numeric_code = code,
                                         is_in_union = is_in_union,
                                         first_year = first_year,
                                         last_year = last_year
  )
  expect_true(is.list(res))
})

