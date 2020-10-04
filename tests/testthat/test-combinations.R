library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("combinations")


test_that("already tidy table", {
  schema <- setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 4) %>%
    setObsVar(name = "production", unit = "t", columns = 5)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_tidy.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("vertical clusters per observed variable with a wide identifying variable", {
  schema <- setCluster(id = "observed", top = c(3, 9), left = 2, height = 4) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 2) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = c(4, 5), row = 1) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(4, 5),
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", unit = "t", columns = c(4, 5),
              key = "cluster", value = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_wide_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("vertical clusters per values variable with two nested wide identifying variables", {
  schema <- setCluster(id = "observed", top = c(4, 8), left = 2, height = 2) %>%
    setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 2) %>%
    setIDVar(name = "year", columns = c(3, 5), row = 1) %>%
    setIDVar(name = "commodities", columns = c(3:6), row = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3:6),
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", unit = "t", columns = c(3:6),
              key = "cluster", value = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_clust_wide_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("bring one wide identifying variable into long form and unlist observed variable", {
  schema <- setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(4, 5), row = 1) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(4, 5),
              key = "dimension", value = "harvested") %>%
    setObsVar(name = "production", unit = "t", columns = c(4, 5),
              key = "dimension", value = "production")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_listed_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

# bring several wide identifying variable into long form and spread long table ----
test_that("several wide identifying variable into long form and unlist observed variable", {
  schema <- setHeader(rows = c(1, 2)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(3, 5), row = 1) %>%
    setIDVar(name = "commodities", columns = c(3:6), row = 2) %>%
    setObsVar(name = "harvested", unit = "ha", columns = c(3:6),
              key = "dimension", value = "harvested") %>%
    setObsVar(name = "production", unit = "t", columns = c(3:6),
              key = "dimension", value = "production")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package="tabshiftr",
                                       mustWork = TRUE), "/table_wide_listed_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})
