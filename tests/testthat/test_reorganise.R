library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("reorganise")


test_that("recognise several vertical clusters of otherwise tidy data", {
  schema <- list(clusters = list(top = c(2, 9), left = 1, width = NULL, height = NULL,
                                   id = "territories", header = FALSE),
                   variables = list(territories =
                                      list(type = "id", name = NULL, form = "long",
                                           row = NULL, col = 1, split = NULL, rel = FALSE),
                                    period =
                                      list(type = "id", name = "year", form = "long",
                                           row = NULL, col = 2, split = NULL, rel = FALSE),
                                    commodities =
                                      list(type = "id", name = NULL, form = "long",
                                           row = NULL, col = 3, split = NULL, rel = FALSE),
                                    harvested =
                                      list(type = "values", unit = "ha", factor = 1,
                                           row = NULL, col = 4, rel = FALSE,
                                           id = NULL, level = NULL),
                                    production =
                                      list(type = "values", unit = "t", factor = 1,
                                           row = NULL, col = 5, rel = FALSE,
                                           id = NULL, level = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr", mustWork = TRUE), "/table1.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("recognise several horizontal clusters of otherwise tidy data", {
  schema <- list(clusters = list(top = 2, left = c(2, 5), width = NULL, height = NULL,
                                id = "territories", header = FALSE),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "wide",
                                        row = 2, col = c(2, 5), split = NULL, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 1, split = NULL, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = c(2, 5), split = NULL, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = c(3, 6), rel = FALSE,
                                        id = NULL, level = NULL),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = c(4, 7), rel = FALSE,
                                        id = NULL, level = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table2.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("rename variables, in already tidy table", {
  schema <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL, header = TRUE),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, split = NULL, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, split = NULL, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 3, split = NULL, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = 4, rel = FALSE,
                                        id = NULL, level = NULL),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = 5, rel = FALSE,
                                        id = NULL, level = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table3.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("spread long table", {
  schema <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL, header = TRUE),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, split = NULL, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, split = NULL, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 3, split = NULL, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = 5, rel = FALSE,
                                        id = "dimension", level = "harvested"),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = 5, rel = FALSE,
                                        id = "dimension", level = "production")))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table4.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("bring wide identifying variable into long form", {
  schema <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL, header = FALSE),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, split = NULL, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, split = NULL, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "wide",
                                        row = 2, col = NULL, split = NULL, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = c(3, 4), rel = FALSE,
                                        id = NULL, level = NULL),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = c(5, 6), rel = FALSE,
                                        id = NULL, level = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table5.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("bring wide identifying variable into long form and spread long table", {
  schema <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                id = NULL, header = TRUE),
                variables = list(territories =
                                   list(type = "id", name = NULL, form = "long",
                                        row = NULL, col = 1, split = NULL, rel = FALSE),
                                 period =
                                   list(type = "id", name = "year", form = "long",
                                        row = NULL, col = 2, split = NULL, rel = FALSE),
                                 commodities =
                                   list(type = "id", name = NULL, form = "wide",
                                        row = NULL, col = c(4, 5), split = NULL, rel = FALSE),
                                 harvested =
                                   list(type = "values", unit = "ha", factor = 1,
                                        row = NULL, col = 3, rel = FALSE,
                                        id = "dimension", level = "harvested"),
                                 production =
                                   list(type = "values", unit = "t", factor = 1,
                                        row = NULL, col = 3, rel = FALSE,
                                        id = "dimension", level = "production")))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table6.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("bring several wide identifying variables into long form", {
  schema <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = NULL, header = FALSE),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = 1, split = NULL, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "wide",
                                       row = 1, col = c(2, 6), split = NULL, rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "wide",
                                       row = 2, col = c(2, 4, 6, 8), split = NULL, rel = FALSE),
                                dimension =
                                  list(type = "id", name = NULL, form = "wide",
                                       row = 3, col = NULL, split = NULL, rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = c(2, 4, 6, 8), rel = FALSE,
                                       id = "dimension", level = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = c(3, 5, 7, 9), rel = FALSE,
                                       id = "dimension", level = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table7.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("split a column that contains several variables in an already tidy table", {
  schema <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                  id = NULL, header = TRUE),
                  variables = list(territories =
                                     list(type = "id", name = NULL, form = "long",
                                          row = NULL, col = 1, split = NULL, rel = FALSE),
                                   period =
                                     list(type = "id", name = "year", form = "long",
                                          row = NULL, col = 2, split = ".+?(?=_)", rel = FALSE),
                                   commodities =
                                     list(type = "id", name = NULL, form = "long",
                                          row = NULL, col = 2, split = "(?<=\\_).*", rel = FALSE),
                                   harvested =
                                     list(type = "values", unit = "ha", factor = 1,
                                          row = NULL, col = 3, rel = FALSE,
                                          id = NULL, level = NULL),
                                   production =
                                     list(type = "values", unit = "t", factor = 1,
                                          row = NULL, col = 4, rel = FALSE,
                                          id = NULL, level = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="rectr"), "/table8.csv"),
                     col_names = FALSE) %>%
    reorganise(schema = schema)

  expect_tibble(x = input, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(input), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = input$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = input$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = input$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = input$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = input$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
})

test_that("Error if arguments have wrong value", {

})

