library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("clusters")

test_that("several vertical clusters of otherwise tidy data", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(3, 10), col = 2, id = "territories"),
         header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", row = c(2, 9), col = 1),
                year =
                  list(type = "id", col = 2),
                commodities =
                  list(type = "id", col = 3),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = 4),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 5))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("relative column positions are valid", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(3, 10), col = 2, width = NULL, height = 4,
                id = "territories"),
         header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", row = c(2, 9), col = 1),
                year =
                  list(type = "id", col = 1, rel = TRUE),
                commodities =
                  list(type = "id", col = 2, rel = TRUE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       col = 3, rel = TRUE),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = 4, rel = TRUE))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)
})

test_that("several horizontal clusters of otherwise tidy data", {
  schema <- makeSchema(
    list(clusters =
           list(row = 2, col = c(2, 5), id = "territories"),
         header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", row = 2, col = c(2, 5)),
                year =
                  list(type = "id", col = 1),
                commodities =
                  list(type = "id", col = c(2, 5)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = c(3, 6)),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = c(4, 7)))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("vertical clusters that are aggregated per measured variable", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(3, 13), col = 2, height = 8, id = "measured"),
         header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 2),
                year =
                  list(type = "id", col = 3),
                commodities =
                  list(type = "id", col = 4),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = 5,
                       key = "cluster", value = 1),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 5,
                       key = "cluster", value = 2))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})