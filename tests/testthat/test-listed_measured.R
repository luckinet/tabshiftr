library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("listed")


# spread long table ----
test_that("unlist measured variable", {
  # without random other columns
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
                  list(type = "measured", unit = "ha", factor = 1,
                       col = 5, key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       col = 5, key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_listed_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # with a couple of other columns
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
                  list(type = "measured", unit = "ha", factor = 1, col = 6,
                       key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 6,
                       key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_listed_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})