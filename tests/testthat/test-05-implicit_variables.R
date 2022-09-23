library(tabshiftr)
library(testthat)
library(checkmate)
context("implicit")


test_that("set an implicit variable in a tidy table", {

  # without clusters or filtering
  input <- tabs2shift$implicit_variable
  input <- input[c(4:8),]

  schema <- setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = 1) %>%
    setObsVar(name = "harvested", columns = 2) %>%
    setObsVar(name = "production", columns = 3)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 1)

})


test_that("set an implicit variable in a wide table (that is only 1 row high)", {

  input <- tabs2shift$two_wide_id
  input <- input[-5,-c(1)]

  schema <-
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = c(1, 5), rows = 1) %>%
    setIDVar(name = "commodities", columns = c(1, 3, 5, 7), rows = 2) %>%
    setObsVar(name = "harvested", columns = c(1, 3, 5, 7), top = 3) %>%
    setObsVar(name = "production", columns = c(2, 4, 6, 8), top = 3)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 1)

})


test_that("set several identifying variables as value", {

  input <- tabs2shift$implicit_variable
  input <- input[c(4:8),]

  schema <- setIDVar(name = "region", value = "group 1") %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = 1) %>%
    setObsVar(name = "harvested", columns = 2) %>%
    setObsVar(name = "production", columns = 3)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 1, groups = TRUE)

})

test_that("set an id variable from the cluster id", {

  input <- tabs2shift$implicit_variable

  schema <- setCluster(id = "territories",
                       left = 1, top = 4) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = 1) %>%
    setObsVar(name = "harvested", columns = 2) %>%
    setObsVar(name = "production", columns = 3)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 1)

})

