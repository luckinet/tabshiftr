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


test_that("filter columns", {

  input <- tabs2shift$tidy

  schema <-
    setFilter(columns = c(1, 2, 3, 5, 6)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

  input <- tabs2shift$one_wide_id_alt2

  schema <-
    setFilter(columns = c(1:4, 6:7, 9:10)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = c(4:9), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(4:6), top = 2) %>%
    setObsVar(name = "production", columns = c(7:9), top = 2)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("filter on several columns", {

  input <- tabs2shift$messy_rows

  schema <-
    setFilter(rows = .find(by = "unit 2", col = 1)) %>%
    setFilter(rows = .find(by = "year 2", col = 2), operator = `&`) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  out <- reorganise(input = input, schema = schema)
  expect_equal(out$territories, c("unit 2", "unit 2"))
  expect_equal(out$year, c("year 2", "year 2"))
  expect_equal(out$commodities, c("maize", "soybean"))
  expect_equal(out$harvested, c(2221, 2211))
  expect_equal(out$production, c(2222, 2212))

  # set several filters without the .find function
  schema <-
    setFilter(rows = 13:16) %>%
    setFilter(rows = c(9, 10, 15, 16), operator = `&`) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  out <- reorganise(input = input, schema = schema)
  expect_equal(out$territories, c("unit 2", "unit 2"))
  expect_equal(out$year, c("year 2", "year 2"))
  expect_equal(out$commodities, c("maize", "soybean"))
  expect_equal(out$harvested, c(2221, 2211))
  expect_equal(out$production, c(2222, 2212))

  # combination of position and .find function
  schema <-
    setFilter(rows = c(13:16)) %>%
    setFilter(rows = .find(by = "year 2", col = 2), operator = `&`) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  out <- reorganise(input = input, schema = schema)
  expect_equal(out$territories, c("unit 2", "unit 2"))
  expect_equal(out$year, c("year 2", "year 2"))
  expect_equal(out$commodities, c("maize", "soybean"))
  expect_equal(out$harvested, c(2221, 2211))
  expect_equal(out$production, c(2222, 2212))

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

test_that("mask out a couple of rows and columns that don't contain target data", {

  input <- tabs2shift$implicit_variable

  schema <- setFilter(rows = c(5:8)) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = 1) %>%
    setObsVar(name = "harvested", columns = 2) %>%
    setObsVar(name = "production", columns = 3)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 1)

})


