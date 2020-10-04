library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("mismatch")


test_that("split a column that contains several variables in one column", {
  schema <- setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2, split = ".+?(?=_)") %>%
    setIDVar(name = "commodities", columns = 2, split = "(?<=\\_).*") %>%
    setObsVar(name = "harvested", unit = "ha", columns = 3) %>%
    setObsVar(name = "production", unit = "t", columns = 4)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("recognise an identifying variable that is actually a merge of two columns", {
  schema <- setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 3), merge = " ") %>%
    setIDVar(name = "commodities", columns = 4) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 5) %>%
    setObsVar(name = "production", unit = "t", columns = 6)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("recognise a distinct variable that is not valid for every cluster", {
  schema <- setCluster(id = "territories", top = c(1, 8, 8), left = c(1, 1, 4),
                       width = 3, height = 6) %>%
    setHeader(rows = 1, relative = TRUE) %>%
    setIDVar(name = "territories", columns = 1, row = 2, relative = TRUE) %>%
    setIDVar(name = "year", columns = 4, row = c(3:6), distinct = TRUE) %>%
    setIDVar(name = "commodities", columns = 1, relative = TRUE) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 2, relative = TRUE) %>%
    setObsVar(name = "production", unit = "t", columns = 3, relative = TRUE)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 3)
})

test_that("set an id variable manually that is not in the table", {
  schema <- setCluster(id = "year", top = c(2, 6), left = 1) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 1, row = c(2, 6)) %>%
    setIDVar(name = "commodities", columns = 2, relative = TRUE) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 3) %>%
    setObsVar(name = "production", unit = "t", columns = 4)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 1)
})

test_that("set an id variable that is not in the table manually", {
  schema <- setCluster(id = "territories", top = 4, left = 1) %>%
    setHeader(rows = 1, relative = TRUE) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 4, relative = TRUE) %>%
    setIDVar(name = "commodities", columns = 1, relative = TRUE) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 2, relative = TRUE) %>%
    setObsVar(name = "production", unit = "t", columns = 3, relative = TRUE)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 1)
})
