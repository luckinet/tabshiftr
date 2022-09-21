library(tabshiftr)
library(testthat)
library(checkmate)
context("listed")


test_that("listed observed variable", {

  input <- tabs2shift$listed_column

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 7,
              key = 6, value = "harvested") %>%
    setObsVar(name = "production", columns = 7,
              key = 6, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

  # different names for listed observed variables
  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "HARV", columns = 7,
              key = 6, value = "harvested") %>%
    setObsVar(name = "PROD", columns = 7,
              key = 6, value = "production")

  out <- reorganise(input = input, schema = schema)
  expect_equal(names(out), c("territories", "year", "commodities", "HARV", "PROD"))

  # keep duplicated observations (perhaps for a good reason)
  input <- tabs2shift$listed_column
  input <- bind_rows(input, input[c(17:18),])

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 7,
              key = 6, value = "harvested") %>%
    setObsVar(name = "production", columns = 7,
              key = 6, value = "production")

  expect_warning(out <- reorganise(input = input, schema = schema))
  expect_tibble(x = out, nrows = 8, ncols = 5)
  expect_true(all(out$production[7:8] == c(4444, 4424)))

})


test_that("listed observed variable and one implicit variable", {

  input <- tabs2shift$listed_column
  input <- input[,-1]
  input <- input[c(1:9),]

  schema <-
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 1) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = 6,
              key = 5, value = "harvested") %>%
    setObsVar(name = "production", columns = 6,
              key = 5, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 1)

})


test_that("bring one wide identifying variable into long form and unlist observed variable", {

  input <- tabs2shift$listed_column_wide

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("bring one wide identifying variable into long form and select only a subset of the long variable", {

  input <- tabs2shift$listed_column_wide

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2, variables = "production")

})


test_that("several wide identifying variable into long form and unlist observed variable", {

  input <- tabs2shift$listed_column_two_wide

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(5, 7), rows = 2) %>%
    setIDVar(name = "commodities", columns = c(5:8), rows = 3) %>%
    setObsVar(name = "harvested", columns = c(5:8),
              key = 3, value = "harvested") %>%
    setObsVar(name = "production", columns = c(5:8),
              key = 3, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

