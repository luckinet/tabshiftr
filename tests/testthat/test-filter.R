library(tabshiftr)
library(testthat)
library(checkmate)
context("filter")


test_that("filter rows", {

  input <- tabs2shift$messy_rows

  schema <-
    setFilter(rows = c(1, 7:10, 13:16)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})
