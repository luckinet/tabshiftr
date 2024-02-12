library(tabshiftr)
library(testthat)
library(checkmate)
context("nested")


test_that("identifying variable that is nested into another identifying variable", {

  input <- tabs2shift$nested_variable

  schema <- setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "sublevel", columns = 2) %>%
    setIDVar(name = "year", columns = 8) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 6) %>%
    setObsVar(name = "production", columns = 7)

  out <- reorganise(input = input, schema = schema)

  expect_equal(out$territories, c("group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 2", "group 2", "group 2", "group 2"))
  expect_equal(out$sublevel, c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2", "unit 3", "unit 3", "unit 3", "unit 3"))
  expect_equal(out$year, c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_equal(out$commodities, c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  expect_equal(out$harvested, c(1111, 1121, 1211, 1221, 2111, 2121, 2211, 2221, 3111, 3121, 3211, 3221))
  expect_equal(out$production, c(1112, 1122, 1212, 1222, 2112, 2122, 2212, 2222, 3112, 3122, 3212, 3222))

})
