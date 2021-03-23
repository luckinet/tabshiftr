library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("wide_id")


test_that("one wide identifying variable into long form", {
  # ... with indices for observed and identifying variables
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 4, 5, 6), rows = 1) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 4), rows = 2) %>%
    setObsVar(name = "production", unit = "t", columns = c(5, 6), rows = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables
  # schema <- setHeader(rows = c(1, 2)) %>%
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "year", columns = 2) %>%
  #   setIDVar(name = "commodities", columns = c(3, 4, 5, 6), rows = 1) %>%
  #   setObsVar(name = "harvested", unit = "ha", columns = , rows = 2) %>%
  #   setObsVar(name = "production", unit = "t", columns = , rows = 2)
  #
  # input <- read_csv(paste0(system.file("test_datasets",
  #                                      package="tabshiftr",
  #                                      mustWork = TRUE), "/table_wide_1.csv"),
  #                   col_names = FALSE)
  # output <- reorganise(input = input, schema = schema)
  #
  # expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables
  # schema <- setHeader(rows = c(1, 2)) %>%
  #   setIDVar(name = "territories", columns = ) %>%
  #   setIDVar(name = "year", columns = ) %>%
  #   setIDVar(name = "commodities", columns = , rows = 1) %>%
  #   setObsVar(name = "harvested", unit = "ha", columns = c(3, 4), rows = 2) %>%
  #   setObsVar(name = "production", unit = "t", columns = c(5, 6), rows = 2)
  #
  # input <- read_csv(paste0(system.file("test_datasets",
  #                                      package="tabshiftr",
  #                                      mustWork = TRUE), "/table_wide_1.csv"),
  #                   col_names = FALSE)
  # output <- reorganise(input = input, schema = schema)
  #
  # expect_valid_table(x = output, units = 2)

})


test_that("wide variable in first rows of header, values separated", {
  # ... with indices for observed and identifying variables
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 5), rows = 1) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 5), rows = 2) %>%
    setObsVar(name = "production", unit = "t", columns = c(4, 6), rows = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables
  # schema <- setHeader(rows = c(1, 2)) %>%
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "year", columns = 2) %>%
  #   setIDVar(name = "commodities", columns = c(3, 5), rows = 1) %>%
  #   setObsVar(name = "harvested", unit = "ha", columns = , rows = 2) %>%
  #   setObsVar(name = "production", unit = "t", columns = , rows = 2)
  #
  # input <- read_csv(paste0(system.file("test_datasets",
  #                                      package="tabshiftr",
  #                                      mustWork = TRUE), "/table_wide_2.csv"),
  #                   col_names = FALSE)
  # output <- reorganise(input = input, schema = schema)
  #
  # expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables
  # schema <- setHeader(rows = c(1, 2)) %>%
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "year", columns = 2) %>%
  #   setIDVar(name = "commodities", columns = c(3, 5), rows = 1) %>%
  #   setObsVar(name = "harvested", unit = "ha", columns = , rows = 2) %>%
  #   setObsVar(name = "production", unit = "t", columns = , rows = 2)
  #
  # input <- read_csv(paste0(system.file("test_datasets",
  #                                      package="tabshiftr",
  #                                      mustWork = TRUE), "/table_wide_2.csv"),
  #                   col_names = FALSE)
  # output <- reorganise(input = input, schema = schema)
  #
  # expect_valid_table(x = output, units = 2)

})


test_that("wide variable in second rows of header, values next to each other", {
  # ... with indices for observed and identifying variables
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3:6), rows = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 4), rows = 1) %>%
    setObsVar(name = "production", unit = "t", columns = c(5, 6), rows = 1)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

})


test_that("wide variable in second rows of header, values spearated", {
  # ... with indices for observed and identifying variables
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 5), rows = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3, 5), rows = 1) %>%
    setObsVar(name = "production", unit = "t", columns = c(4, 6), rows = 1)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables
  # schema <- setHeader(rows = c(1, 2)) %>%
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "year", columns = 2) %>%
  #   setIDVar(name = "commodities", columns = c(3, 5), rows = 2) %>%
  #   setObsVar(name = "harvested", unit = "ha", columns = c(3, 5), rows = 1) %>%
  #   setObsVar(name = "production", unit = "t", columns = c(4, 6), rows = 1)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables
  # schema <- setHeader(rows = c(1, 2)) %>%
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "year", columns = 2) %>%
  #   setIDVar(name = "commodities", columns = c(3, 5), rows = 2) %>%
  #   setObsVar(name = "harvested", unit = "ha", columns = c(3, 5), rows = 1) %>%
  #   setObsVar(name = "production", unit = "t", columns = c(4, 6), rows = 1)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

})

test_that("several wide identifying variables", {
  # ... with indices for observed and identifying variables
  schema <- setHeader(rows = c(1:3)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 6), rows = 1) %>%
    setIDVar(name = "commodities", columns = c(2, 4, 6, 8), rows = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(2, 4, 6, 8), rows = 3) %>%
    setObsVar(name = "production", unit = "t", columns = c(3, 5, 7, 9), rows = 3)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables
  schema <- setHeader(rows = c(1:3)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 6), rows = 1) %>%
    setIDVar(name = "commodities", columns = c(2, 4, 6, 8), rows = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = find_col("harv*"), rows = 3) %>%
    setObsVar(name = "production", unit = "t", columns = find_col("prod*"), rows = 3)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables
  schema <- setHeader(rows = c(1:3)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 6), rows = 1) %>%
    setIDVar(name = "commodities", columns = find_col("soy|mai*"), rows = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(2, 4, 6, 8), rows = 3) %>%
    setObsVar(name = "production", unit = "t", columns = c(3, 5, 7, 9), rows = 3)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for the header
  schema <- setHeader(rows = find_row("year|soy|mai*|harv*|prod*")) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 6), rows = 1) %>%
    setIDVar(name = "commodities", columns = c(2, 4, 6, 8), rows = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(2, 4, 6, 8), rows = 3) %>%
    setObsVar(name = "production", unit = "t", columns = c(3, 5, 7, 9), rows = 3)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for everything
  schema <- setHeader(rows = find_row("year|soy|mai*|harv*|prod*")) %>%
    setIDVar(name = "territories", columns = find_col("unit*")) %>%
    setIDVar(name = "year", columns = find_row("year*"), rows = find_row("year*")) %>%
    setIDVar(name = "commodities", columns = find_col("soy|mai*"), rows = find_col("soy|mai*")) %>%
    setObsVar(name = "harvested", unit = "ha", columns = find_col("harv*"), rows = find_row("harv*")) %>%
    setObsVar(name = "production", unit = "t", columns = find_col("prod*"), rows = find_row("prod*"))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})
