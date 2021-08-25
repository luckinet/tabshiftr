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

test_that("filter rows from clusters", {

  input <- tabs2shift$clusters_nested

  schema <- setCluster(id = "territories",
                       left = 1, top = c(3, 8)) %>%
    setFilter(rows = c(1:12)) %>%
    setIDVar(name = "territories", columns = 1, rows = c(3, 8)) %>%
    setIDVar(name = "year", columns = 7) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("filter rows from clusters with listed observed variables", {

  input <- tabs2shift$listed_column_wide

  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setFilter(rows = c(1, 4, 5, 9, 10)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  out <- reorganise(input = input, schema = schema)
  expect_equal(out$territories, c("unit 1", "unit 1", "unit 2", "unit 2"))
  expect_equal(out$year, c("year 2", "year 2", "year 2", "year 2"))
  expect_equal(out$commodities, c("maize", "soybean", "maize", "soybean"))
  expect_equal(out$harvested, c(1221, 1211, 2221, 2211))
  expect_equal(out$production, c(1222, 1212, 2222, 2212))

})



