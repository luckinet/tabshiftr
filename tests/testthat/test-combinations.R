library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("combinations")


test_that("already tidy table", {
  schema <- makeSchema(
    list(header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 1),
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
                                       mustWork = TRUE), "/table_tidy.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("vertical clusters per measured variable with a wide identifying variable", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(3, 9), col = 2, height = 4, id = "measured"),
         header = list(row = 1),
         meta = list(del = NULL, dec = NULL, na = NULL),
         variables =
           list(territories =
                  list(type = "id", col = 2),
                year =
                  list(type = "id", col = 3),
                commodities =
                  list(type = "id", row = 1, col = c(4, 5)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       col = c(4, 5), key = "cluster", value = 1),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = c(4, 5), key = "cluster", value = 2))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_wide_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("vertical clusters per values variable with two nested wide identifying variables", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(4, 8), col = 2, height = 2,
                id = "measured"),
         header = list(row = c(1, 2)),
         variables =
           list(territories =
                  list(type = "id", row = NULL, col = 2),
                year =
                  list(type = "id", row = 1, col = c(3, 5)),
                commodities =
                  list(type = "id", row = 2, col = c(3:6)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = c(3:6),
                       key = "cluster", value = 1),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = c(3:6),
                       key = "cluster", value = 2))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_wide_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("bring one wide identifying variable into long form and unlist measured variable", {
  schema <- makeSchema(
    list(header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", row = NULL, col = 2),
                commodities =
                  list(type = "id", row = 1, col = c(4, 5)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       col = c(4, 5), key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = c(4, 5), key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_listed_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring several wide identifying variable into long form and spread long table ----
test_that("several wide identifying variable into long form and unlist measured variable", {
  schema <- makeSchema(
    list(header = list(row = c(1, 2)),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", row = 1, col = c(3, 5)),
                commodities =
                  list(type = "id", row = 2, col = c(3, 4, 5, 6)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = c(3, 4, 5, 6),
                       key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = c(3, 4, 5, 6),
                       key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_listed_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})