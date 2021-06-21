library(tabshiftr)
library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("wide_id")


test_that("one wide identifying variable into long form", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/one_wide_variable_1.csv"),
                    col_names = FALSE)

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = c(4:7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(4, 5), top = 2) %>%
    setObsVar(name = "production", columns = c(6, 7), top = 2)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("wide variable in first row of header", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/one_wide_variable_2.csv"),
                    col_names = FALSE)

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = c(4, 6), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(4, 6), top = 2) %>%
    setObsVar(name = "production", columns = c(5, 7), top = 2)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("wide variable in second row of header", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/one_wide_variable_3.csv"),
                    col_names = FALSE)

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3:6), rows = 2) %>%
    setObsVar(name = "harvested", columns = c(3, 4)) %>%
    setObsVar(name = "production", columns = c(5, 6))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("wide variable in second rows of header, values spearated", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/one_wide_variable_4.csv"),
                    col_names = FALSE)

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3, 5), rows = 2) %>%
    setObsVar(name = "harvested", columns = c(3, 5)) %>%
    setObsVar(name = "production", columns = c(4, 6))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("vertical clusters per observed variable with a wide identifying variable", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/clust_one_wide_variable.csv"),
                    col_names = FALSE)

  schema <- setCluster(id = "observed",
                       left = 2, top = c(2, 8)) %>%
    setIDVar(name = "territories", columns = 3) %>%
    setIDVar(name = "year", columns = 4) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = "cluster", value = 2)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("vertical clusters per observed variable with two nested wide identifying variables", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/clust_two_wide_variable.csv"),
                    col_names = FALSE)

  schema <- setCluster(id = "observed",
                       left = 1, top = c(4, 8)) %>%
    setIDVar(name = "territories", columns = 3) %>%
    setIDVar(name = "year", columns = c(5, 7), rows = 2) %>%
    setIDVar(name = "commodities", columns = c(5:8), rows = 3) %>%
    setObsVar(name = "harvested", columns = c(5:8),
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = c(5:8),
              key = "cluster", value = 2)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("several wide identifying variables", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/two_wide_variables.csv"),
                    col_names = FALSE)

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 6), rows = 1) %>%
    setIDVar(name = "commodities", columns = c(2, 4, 6, 8), rows = 2) %>%
    setObsVar(name = "harvested", columns = c(2, 4, 6, 8), top = 3) %>%
    setObsVar(name = "production", columns = c(3, 5, 7, 9), top = 3)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})
