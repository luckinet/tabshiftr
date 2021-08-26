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
  expect_equal(out$commodities, c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_equal(out$harvested, c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211, 3121, 3111, 3221, 3211))
  expect_equal(out$production, c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212, 3122, 3112, 3222, 3212))

})
