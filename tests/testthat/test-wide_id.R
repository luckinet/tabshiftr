library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("wide_id")


# bring one wide identifying variable into long form ----
test_that("bring one wide identifying variable into long form", {
  # wide variable in first row of header, values next to each other
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 4, 5, 6), row = 1) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 4), row = 2) %>%
    setObsVar(name = "production", unit = "t", columns = c(5, 6), row = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in first row of header, values separated
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 5), row = 1) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 5), row = 2) %>%
    setObsVar(name = "production", unit = "t", columns = c(4, 6), row = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in second row of header, values next to each other
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3:6), row = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 4), row = 1) %>%
    setObsVar(name = "production", unit = "t", columns = c(5, 6), row = 1)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in second row of header, values spearated
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 5), row = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 5), row = 1) %>%
    setObsVar(name = "production", unit = "t", columns = c(4, 6), row = 1)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring several wide identifying variables into long form ----
test_that("bring several wide identifying variables into long form", {
  schema <- setHeader(rows = c(1:3)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 6), row = 1) %>%
    setIDVar(name = "commodities", columns = c(2, 4, 6, 8), row = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(2, 4, 6, 8), row = 3) %>%
    setObsVar(name = "production", unit = "t", columns = c(3, 5, 7, 9), row = 3)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})
