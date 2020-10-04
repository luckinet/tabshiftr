library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("listed")


# spread long table ----
test_that("unlist measured variable", {
  # without random other columns
  schema <- setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 5,
              key = "dimension", value = "harvested") %>%
    setObsVar(name = "production", unit = "t", columns = 5,
              key = "dimension", value = "production")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_listed_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # with a couple of other columns
  schema <- setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 6,
              key = "dimension", value = "harvested") %>%
    setObsVar(name = "production", unit = "t", columns = 6,
              key = "dimension", value = "production")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_listed_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})