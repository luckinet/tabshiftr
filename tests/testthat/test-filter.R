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

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

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

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

  input <- tabs2shift$one_wide_id_alt2

  schema <-
    setFilter(columns = c(1:4, 6:7, 9:10)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = c(4:9), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(4:6), top = 2) %>%
    setObsVar(name = "production", columns = c(7:9), top = 2)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that("filter on several columns", {

  input <- tabs2shift$messy_rows

  schema <-
    setFilter(rows = .find(pattern = "unit 2", col = 1)) %>%
    setFilter(rows = .find(pattern = "year 2", col = 2), operator = `&`) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  out <- reorganise(input = input, schema = schema)
  expect_equal(out$territories, c("unit 2", "unit 2"))
  expect_equal(out$year, c("year 2", "year 2"))
  expect_equal(out$commodities, c("soybean", "maize"))
  expect_equal(out$harvested, c(2211, 2221))
  expect_equal(out$production, c(2212, 2222))

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
  expect_equal(out$commodities, c("soybean", "maize"))
  expect_equal(out$harvested, c(2211, 2221))
  expect_equal(out$production, c(2212, 2222))

  # combination of position and .find function
  schema <-
    setFilter(rows = c(13:16)) %>%
    setFilter(rows = .find(pattern = "year 2", col = 2), operator = `&`) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  out <- reorganise(input = input, schema = schema)
  expect_equal(out$territories, c("unit 2", "unit 2"))
  expect_equal(out$year, c("year 2", "year 2"))
  expect_equal(out$commodities, c("soybean", "maize"))
  expect_equal(out$harvested, c(2211, 2221))
  expect_equal(out$production, c(2212, 2222))

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

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

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
  expect_equal(out$commodities, c("soybean", "maize", "soybean", "maize"))
  expect_equal(out$harvested, c(1211, 1221, 2211, 2221))
  expect_equal(out$production, c(1212, 1222, 2212, 2222))

})

test_that("mask out a couple of rows and columns that don't contain target data", {

  input <- tabs2shift$implicit_variable

  schema <- setFilter(rows = c(5:8)) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = 1) %>%
    setObsVar(name = "harvested", columns = 2) %>%
    setObsVar(name = "production", columns = 3)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 1)

})


