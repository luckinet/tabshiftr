library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("reorganise")

# several vertical clusters of otherwise tidy data ----
test_that("several vertical clusters of otherwise tidy data", {
  schema <- makeSchema(
    list(clusters =
           list(top = c(3, 10), left = 2, id = "territories"),
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
                                       package="rectifyr",
                                       mustWork = TRUE), "/table1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# several horizontal clusters of otherwise tidy data ----
test_that("several horizontal clusters of otherwise tidy data", {
  schema <- makeSchema(
    list(clusters =
           list(top = 2, left = c(2, 5), width = NULL, height = NULL,
                id = "territories"),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = 2, col = c(2, 5), rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = c(2, 5), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = c(3, 6), rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = c(4, 7), rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# already tidy table ----
test_that("already tidy table", {
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 3, rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = 4, rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = 5, rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# bring one wide identifying variable into long form ----
test_that("bring one wide identifying variable into long form", {
  # wide variable in first row of header, values next to each other
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 1, col = c(3, 4, 5, 6), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 2, col = c(3, 4), rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 2, col = c(5, 6), rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table41.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)

  # wide variable in first row of header, values separated
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 1, col = c(3, 5), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 2, col = c(3, 5), rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 2, col = c(4, 6), rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table42.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)

  # wide variable in second row of header, values next to each other
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 2, col = NULL, rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 1, col = c(3, 4), rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 1, col = c(5, 6), rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table43.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)

  # wide variable in secon row of header, values spearated
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 2, col = NULL, rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 1, col = c(3, 5), rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 1, col = c(4, 6), rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table44.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# bring several wide identifying variables into long form ----
test_that("bring several wide identifying variables into long form", {
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1:3), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = 1, col = c(2, 6), rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 2, col = c(2, 4, 6, 8), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = 3, col = c(2, 4, 6, 8), rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = 3, col = c(3, 5, 7, 9), rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# spread long table ----
test_that("spread long table", {
  # without random other columns
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 3, rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = 5, rel = FALSE,
                       key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = 5, rel = FALSE,
                       key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table61.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)

  # with a couple of other columns
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 3, rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = 6, rel = FALSE,
                       key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = 6, rel = FALSE,
                       key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table62.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# bring one wide identifying variable into long form and spread long table ----
test_that("bring one wide identifying variable into long form and spread long table", {
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 1, col = c(4, 5), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = c(4, 5), rel = FALSE,
                       key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = c(4, 5), rel = FALSE,
                       key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table7.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# bring several wide identifying variable into long form and spread long table ----
test_that("bring several wide identifying variable into long form and spread long table", {
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = 1, col = c(3, 5), rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 2, col = c(3, 4, 5, 6), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = c(3, 4, 5, 6), rel = FALSE,
                       key = "dimension", value = "harvested"),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = c(3, 4, 5, 6), rel = FALSE,
                       key = "dimension", value = "production"))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table8.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# split a column that contains several id variables in an already tidy table ----
test_that("split a column that contains several variables in an already tidy table", {
  schema <- makeSchema(
    list(clusters =
           list(top = NULL, left = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = ".+?(?=_)",
                       row = NULL, col = 2, rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = "(?<=\\_).*",
                       row = NULL, col = 2, rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = 3, rel = FALSE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = 4, rel = FALSE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table9.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# vertical clusters that are aggregated per values variable ----
test_that("vertical clusters that are aggregated per values variable", {
  schema <- makeSchema(
    list(clusters =
           list(top = c(3, 13), left = 2, height = 8, id = "measured"),
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
                                       package="rectifyr",
                                       mustWork = TRUE), "/table10.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# vertical clusters per values variable with a wide identifying variable ----
test_that("vertical clusters per values variable with a wide identifying variable", {
  schema <- makeSchema(
    list(clusters =
           list(top = c(3, 9), left = 2, height = 4, id = "measured"),
         header = list(row = 1),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
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
                                       package="rectifyr",
                                       mustWork = TRUE), "/table11.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

# vertical clusters per values variable with a two nested wide identifying variables ----
test_that("vertical clusters per values variable with a two nested wide identifying variables", {
  schema <- makeSchema(
    list(clusters =
           list(top = c(4, 8), left = 2, width = NULL, height = 2,
                id = "measured"),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = 1, col = c(3, 5), rel = FALSE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = 2, col = c(3:6), rel = FALSE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = c(3:6), rel = FALSE,
                       key = "cluster", value = 1),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = c(3:6), rel = FALSE,
                       key = "cluster", value = 2))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table12.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output)
})

test_that("relative values work in all cases", {
  schema <- makeSchema(
    list(clusters =
           list(top = c(3, 10), left = 2, width = NULL, height = 4,
                id = "territories"),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL, types = NULL),
         variables =
           list(territories =
                  list(type = "id", value = NULL, split = NULL,
                       row = c(2, 9), col = 1, rel = FALSE),
                year =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 1, rel = TRUE),
                commodities =
                  list(type = "id", value = NULL, split = NULL,
                       row = NULL, col = 2, rel = TRUE),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1,
                       row = NULL, col = 3, rel = TRUE,
                       key = NULL, value = NULL),
                production =
                  list(type = "measured", unit = "t", factor = 1,
                       row = NULL, col = 4, rel = TRUE,
                       key = NULL, value = NULL))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="rectifyr",
                                       mustWork = TRUE), "/table1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output)
})