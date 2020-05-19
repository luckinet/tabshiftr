library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("wide_id")


# bring one wide identifying variable into long form ----
test_that("bring one wide identifying variable into long form", {
  # wide variable in first row of header, values next to each other
  schema <- makeSchema(
    list(header = list(row = c(1, 2)),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", col = 2),
                commodities =
                  list(type = "id", row = 1, col = c(3, 4, 5, 6)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 2, col = c(3, 4)),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 2, col = c(5, 6)))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in first row of header, values separated
  schema <- makeSchema(
    list(header = list(row = c(1, 2)),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", col = 2),
                commodities =
                  list(type = "id", row = 1, col = c(3, 5)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 2, col = c(3, 5)),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 2, col = c(4, 6)))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in second row of header, values next to each other
  schema <- makeSchema(
    list(header = list(row = c(1, 2)),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", col = 2),
                commodities =
                  list(type = "id", row = 2),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 1, col = c(3, 4)),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 1, col = c(5, 6)))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in second row of header, values spearated
  schema <- makeSchema(
    list(header = list(row = c(1, 2)),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", col = 2),
                commodities =
                  list(type = "id", row = 2),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 1, col = c(3, 5)),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 1, col = c(4, 6)))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring several wide identifying variables into long form ----
test_that("bring several wide identifying variables into long form", {
  schema <- makeSchema(
    list(header = list(row = c(1:3)),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", row = 1, col = c(2, 6)),
                commodities =
                  list(type = "id", row = 2, col = c(2, 4, 6, 8)),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 3, col = c(2, 4, 6, 8)),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 3, col = c(3, 5, 7, 9)))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})
