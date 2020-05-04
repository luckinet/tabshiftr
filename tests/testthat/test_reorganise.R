library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("reorganise")

# several vertical clusters of otherwise tidy data ----
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
                                       mustWork = TRUE), "/table1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("relative values work in all cases", {
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
                                       mustWork = TRUE), "/table1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)
})

# several horizontal clusters of otherwise tidy data ----
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
                                       mustWork = TRUE), "/table2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# already tidy table ----
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
                                       mustWork = TRUE), "/table3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring one wide identifying variable into long form ----
test_that("bring one wide identifying variable into long form", {
  # wide variable in first row of header, values next to each other
  schema <- makeSchema(
    list(clusters =
           list(row = NULL, col = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL),
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
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table41.csv"),
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
                                       mustWork = TRUE), "/table42.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in second row of header, values next to each other
  schema <- makeSchema(
    list(clusters =
           list(row = NULL, col = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL),
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
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table43.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # wide variable in second row of header, values spearated
  schema <- makeSchema(
    list(clusters =
           list(row = NULL, col = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL),
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
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table44.csv"),
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
                                       mustWork = TRUE), "/table5.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# spread long table ----
test_that("spread long table", {
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
                                       mustWork = TRUE), "/table61.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)

  # with a couple of other columns
  schema <- makeSchema(
    list(clusters =
           list(row = NULL, col = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL),
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
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table62.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring one wide identifying variable into long form and spread long table ----
test_that("bring one wide identifying variable into long form and spread long table", {
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
                                       mustWork = TRUE), "/table7.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring several wide identifying variable into long form and spread long table ----
test_that("bring several wide identifying variable into long form and spread long table", {
  schema <- makeSchema(
    list(clusters =
           list(row = NULL, col = NULL, width = NULL, height = NULL,
                id = NULL),
         header = list(row = c(1, 2), rel = FALSE),
         meta = list(del = NULL, dec = NULL, na = NULL),
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
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table8.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# split a column that contains several id variables in an already tidy table ----
test_that("split a column that contains several variables in an already tidy table", {
  schema <- makeSchema(
    list(header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 1),
                year =
                  list(type = "id", split = ".+?(?=_)", col = 2),
                commodities =
                  list(type = "id", split = "(?<=\\_).*", col = 2),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = 3),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 4))))

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table9.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# vertical clusters that are aggregated per values variable ----
test_that("vertical clusters that are aggregated per values variable", {
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
                                       mustWork = TRUE), "/table10.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# vertical clusters per values variable with a wide identifying variable ----
test_that("vertical clusters per values variable with a wide identifying variable", {
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
                                       mustWork = TRUE), "/table11.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# vertical clusters per values variable with two nested wide identifying variables ----
test_that("vertical clusters per values variable with a two nested wide identifying variables", {
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
                                       mustWork = TRUE), "/table12.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# recognise a distinct variable that is not valid for every cluster ----
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
                                       mustWork = TRUE), "/table13.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 3)
})

# set an id variable, which is not in the table, manually ----
test_that("set a id variable that is not in the table manually", {
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
                                       mustWork = TRUE), "/table14.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 1)
})

# set an id variable, which is clusterID and that is also not in the table, manually ----
test_that("set a id variable that is not in the table manually", {
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
                                       mustWork = TRUE), "/table15.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 1)
})