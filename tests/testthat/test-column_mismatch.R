library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("mismatch")


test_that("split a column that contains several variables in one columne", {
  schema <- makeSchema(
    list(header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", col = 2, split = ".+?(?=_)"),
                commodities =
                  list(type = "id", col = 2, split = "(?<=\\_).*"),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = 3),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 4))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("recognise an identifying variable that is actually a merge of two columns", {
  schema <- makeSchema(
    list(header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", col = c(2, 3), merge = " "),
                commodities =
                  list(type = "id", col = 4),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = 5),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 6))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("recognise a distinct variable that is not valid for every cluster", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(1, 8, 8), col = c(1, 1, 4), width = 3, height = 6,
                id = "territories"),
         meta = list(del = NULL, dec = NULL, na = NULL),
         header = list(row = 1, rel = TRUE),
         variables =
           list(territories =
                  list(type = "id", row = 1, col = 1, rel = TRUE),
                year =
                  list(type = "id", row = c(3:6), col = 4, dist = TRUE),
                commodities =
                  list(type = "id", col = 1, rel = TRUE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       col = 2, rel = TRUE),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = 3, rel = TRUE))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 3)
})

test_that("set a id variable manually that is not in the table", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(2, 6), col = 1, id = "year"),
         header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", value = "unit 1"),
                year =
                  list(type = "id", row = c(2, 6), col = 1),
                commodities =
                  list(type = "id", col = 2, rel = TRUE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       col = 3),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = 4))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 1)
})

test_that("set an id variable that is not in the table manually", {
  schema <- makeSchema(
    list(clusters =
           list(row = 4, col = 1, id = "territories"),
         header = list(row = 1, rel = TRUE),
         variables =
           list(territories =
                  list(type = "id", value = "unit 1"),
                year =
                  list(type = "id", col = 4, rel = TRUE),
                commodities =
                  list(type = "id", col = 1, rel = TRUE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       col = 2, rel = TRUE),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = 3, rel = TRUE))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_mismatch_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 1)
})
