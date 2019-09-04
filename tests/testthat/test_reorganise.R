library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("reorganise")


test_that("recognise several vertical clusters of otherwise tidy data", {
  schema1 <- list(clusters = list(top = c(2, 9), left = 1, width = NULL, height = NULL,
                                   id = "territories"),
                   variables = list(territories =
                                      list(type = "id", name = NULL, form = "long",
                                           row = NULL, col = 1, rel = FALSE),
                                    period =
                                      list(type = "id", name = "year", form = "long",
                                           row = NULL, col = 2, rel = FALSE),
                                    commodities =
                                      list(type = "id", name = NULL, form = "long",
                                           row = NULL, col = 3, rel = FALSE),
                                    harvested =
                                      list(type = "values", unit = "ha", factor = 1,
                                           row = NULL, col = 4, rel = FALSE,
                                           id = NULL, level = NULL),
                                    production =
                                      list(type = "values", unit = "t", factor = 1,
                                           row = NULL, col = 5, rel = FALSE,
                                           id = NULL, level = NULL)))

  input1 <- read_csv(paste0(system.file("test_datasets", package="rectr", mustWork = TRUE), "/table1.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema1)

  expect_tibble(x = input1, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input1), permutation.of = c("territories", "year", "commodities", "harvested", "production"))
  expect_identical(object = input1$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input1$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input1$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  expect_identical(object = input1$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  expect_identical(object = input1$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("recognise several horizontal clusters of otherwise tidy data", {
  schema2 <- list(clusters = list(top = 2, left = c(2, 5), width = NULL, height = NULL,
                                id = "territories"),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "wide",
                                        row = 2, col = c(2, 5), rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 1, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = c(2, 5), rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = c(3, 6), rel = FALSE,
                                        id = NULL, level = NULL),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = c(4, 7), rel = FALSE,
                                        id = NULL, level = NULL)))

  input2 <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table2.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema2)

  expect_tibble(x = input2, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input2), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input2$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input2$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input2$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  expect_identical(object = input2$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  expect_identical(object = input2$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("rename variables, in already tidy table", {
  schema3 <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 3, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = 4, rel = FALSE,
                                        id = NULL, level = NULL),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = 5, rel = FALSE,
                                        id = NULL, level = NULL)))

  input3 <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table3.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema3)

  expect_tibble(x = input3, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input3), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input3$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input3$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input3$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  expect_identical(object = input3$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  expect_identical(object = input3$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("spread long table", {
  schema4 <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 3, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = 5, rel = FALSE,
                                        id = "dimension", level = "harvested"),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = 5, rel = FALSE,
                                        id = "dimension", level = "production")))

  input4 <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table4.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema4)

  expect_tibble(x = input4, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input4), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input4$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input4$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  # expect_identical(object = input4$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  # expect_identical(object = input4$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  # expect_identical(object = input4$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("bring wide identifying variable into long form", {
  schema5 <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "wide",
                                        row = 2, col = NULL, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = c(3, 4), rel = FALSE,
                                        id = NULL, level = NULL),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = c(5, 6), rel = FALSE,
                                        id = NULL, level = NULL)))

  input5 <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table5.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema5)

  expect_tibble(x = input5, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input5), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input5$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input5$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  # expect_identical(object = input5$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  # expect_identical(object = input5$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  # expect_identical(object = input5$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("bring wide identifying variable into long form and spread long table", {
  schema6 <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "wide",
                                        row = NULL, col = c(4, 5), rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = 3, rel = FALSE,
                                        id = "dimension", level = "harvested"),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = 3, rel = FALSE,
                                        id = "dimension", level = "production")))

  input6 <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table6.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema6)

  expect_tibble(x = input6, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input6), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input6$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input6$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  # for the following uncommented tests, the output of reorganise should probably be resorted accoring to all identifying variables
  # expect_identical(object = input6$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  # expect_identical(object = input6$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  # expect_identical(object = input6$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("bring several wide identifying variables into long form", {
  schema7 <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = NULL),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = 1, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "wide",
                                       row = 1, col = c(2, 6), rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "wide",
                                       row = 2, col = c(2, 4, 6, 8), rel = FALSE),
                                dimension =
                                  list(type = "id", name = NULL, form = "wide",
                                       row = 3, col = NULL, rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = c(2, 4, 6, 8), rel = FALSE,
                                       id = "dimension", level = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = c(3, 5, 7, 9), rel = FALSE,
                                       id = "dimension", level = NULL)))

  input7 <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table7.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = schema7)

  expect_tibble(x = input7, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input7), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input7$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input7$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  # expect_identical(object = input7$commodities, expected = c("soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize"))
  # expect_identical(object = input7$harvested, expected = c("1111", "1121", "1211", "1221", "2111", "2121", "2211", "2221"))
  # expect_identical(object = input7$production, expected = c("1112", "1122", "1212", "1222", "2112", "2122", "2212", "2222"))
})

test_that("Error if arguments have wrong value", {

})
