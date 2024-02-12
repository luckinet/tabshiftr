library(tabshiftr)
library(testthat)
library(checkmate)
context("clusters")

test_that("vertical clusters of otherwise tidy data", {

  input <- tabs2shift$clusters_vertical

  schema <- setCluster(id = "territories",
                       left = 1, top = c(3, 9)) %>%
    setIDVar(name = "territories", columns = 1, rows = c(3, 9)) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 5) %>%
    setObsVar(name = "harvested", columns = 6) %>%
    setObsVar(name = "production", columns = 7)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that("horizontal clusters of otherwise tidy data", {

  input <- tabs2shift$clusters_horizontal

  schema <- setCluster(id = "territories",
                       left = c(1, 6), top = 2) %>%
    setIDVar(name = "territories", columns = c(1, 6), rows = 2) %>%
    setIDVar(name = "year", columns = c(2, 7)) %>%
    setIDVar(name = "commodities", columns = c(1, 6)) %>%
    setObsVar(name = "harvested", columns = c(3, 8)) %>%
    setObsVar(name = "production", columns = c(4, 9))

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})


test_that("vertical cluster with a wide identifying variable (and a single listed observed variable that is missing)", {

  input <- tabs2shift$clusters_one_wide

  schema <- setCluster(id = "territories", left = 1, top = c(2, 5)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(4, 5), rows = 1) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = c(4, 5))

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2, variables = "harvested")

  # input <- tabs2shift$clusters_one_wide_sameColumn
  #
  # schema <-
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "commodities", columns = 4) %>%
  #   setIDVar(name = "year", columns = c(5, 6), rows = 1) %>%
  #   setObsVar(name = "harvested", columns = c(5, 6),
  #             key = 3, value = "harvested")
  #
  # .expect_valid_table(x = reorganise(input = input, schema = schema), variables = "harvested", units = 2)


})


test_that("vertical clusters with a listed observed variable", {

  input <- tabs2shift$listed_column_wide

  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that("vertical clusters with a listed observed variable and an implicit variable", {

  input <- tabs2shift$listed_column_wide

  # territories absolute
  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setIDVar(name = "region", value = "group 1") %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2, groups = TRUE)

  # territories implicit
  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setFilter(rows = c(1, 4, 5, 9, 10)) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  out <- reorganise(input = input, schema = schema)

  expect_equal(out$territories, c("unit 1", "unit 1", "unit 1", "unit 1"))
  expect_equal(out$year, c("year 2", "year 2", "year 2", "year 2"))
  expect_equal(out$commodities, c("soybean", "maize", "soybean", "maize"))
  expect_equal(out$harvested, c(1211, 1221, 2211, 2221))
  expect_equal(out$production, c(1212, 1222, 2212, 2222))

})

test_that("clusters that are aggregated per observed variable", {

  input <- tabs2shift$clusters_observed

  schema <- setCluster(id = "observed",
                       left = 1, top = c(2, 12)) %>%
    setIDVar(name = "territories", columns = 2) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = 5) %>%
    setObsVar(name = "harvested", columns = 7, key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = 7, key = "cluster", value = 2)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})


test_that("vertical clusters of observed variable with a wide identifying variable", {

  input <- tabs2shift$clusters_observed_one_wide

  schema <- setCluster(id = "observed",
                       left = 2, top = c(2, 8)) %>%
    setIDVar(name = "territories", columns = 3) %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = "cluster", value = 2)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})


test_that("vertical clusters of observed variable with two nested wide identifying variables", {

  input <- tabs2shift$clusters_observed_two_wide

  schema <- setCluster(id = "observed",
                       left = 1, top = c(4, 8)) %>%
    setIDVar(name = "territories", columns = 3) %>%
    setIDVar(name = "year", columns = c(5, 7), rows = 2) %>%
    setIDVar(name = "commodities", columns = c(5:8), rows = 3) %>%
    setObsVar(name = "harvested", columns = c(5:8),
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = c(5:8),
              key = "cluster", value = 2)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

