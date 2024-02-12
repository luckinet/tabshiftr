library(tabshiftr)
library(testthat)
library(checkmate)
library(dplyr)
context("tidy")


test_that("already tidy table", {

  input <- tabs2shift$tidy

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})
