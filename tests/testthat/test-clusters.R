library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("clusters")

test_that("several vertical clusters of otherwise tidy data", {
  schema <- setCluster(id = "territories", top = c(3, 8), left = 2) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1, rows = c(2, 7)) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 4, unit = "ha") %>%
    setObsVar(name = "production", columns = 5, unit = "t")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("clusterID is in absolute values, while all other are relative", {
  schema <- setCluster(id = "territories", top = c(3, 8), left = 2, height = 4) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1, rows = c(2, 7)) %>%
    setIDVar(name = "year", columns = 1, relative = TRUE) %>%
    setIDVar(name = "commodities", columns = 2, relative = TRUE) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 3, relative = TRUE) %>%
    setObsVar(name = "production", unit = "t", columns = 4, relative = TRUE)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)
})

test_that("relative columns positions (all) are valid", {
  schema <- setCluster(id = "territories", top = c(2, 7), left = 1, height = 5) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1, rows = 1, relative = TRUE) %>%
    setIDVar(name = "year", columns = 2, relative = TRUE) %>%
    setIDVar(name = "commodities", columns = 3, relative = TRUE) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 4, relative = TRUE) %>%
    setObsVar(name = "production", unit = "t", columns = 5, relative = TRUE)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_1.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)
  expect_valid_table(x = output, units = 2)
})

test_that("several horizontal clusters of otherwise tidy data", {
  schema <- setCluster(id = "territories", left = c(2, 5), top = 2) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = c(2, 5), rows = 2) %>%
    setIDVar(name = "year", columns = 1) %>%
    setIDVar(name = "commodities", columns = c(2, 5)) %>%
    setObsVar(name = "harvested", columns = c(3, 6), unit = "ha") %>%
    setObsVar(name = "production", columns = c(4, 7), unit = "t")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_2.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("vertical clusters that are aggregated per observed variable", {
  schema <- setCluster(id = "observed", left = 2, top = c(3, 12), height = 8) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 2) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = 4) %>%
    setObsVar(name = "harvested", columns = 5, unit = "ha",
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = 5, unit = "t",
              key = "cluster", value = 2)

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_3.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 2)
})

test_that("clusters that are nested into parent clusters", {
  schema <- setCluster(id = "sublevel", left = 1, top = c(3, 8, 14),
                       parent = "territories", member = c(1, 1, 2)) %>%
    setHeader(rows = 1) %>%
    setIDVar(name = "territories", columns = 1, rows = c(2, 13)) %>%
    setIDVar(name = "sublevel", columns = 1, rows = c(3, 8, 14)) %>%
    setIDVar(name = "year", columns = 5) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = 3, unit = "ha") %>%
    setObsVar(name = "production", columns = 4, unit = "t")

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_4.csv"),
                    col_names = FALSE)

  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 3, parents = TRUE)


  # ... with regular expressions for observed variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 3, parents = TRUE)


  # ... with regular expressions for identifying variables

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/table_clust_4.csv"),
                    col_names = FALSE)
  output <- reorganise(input = input, schema = schema)

  expect_valid_table(x = output, units = 3, parents = TRUE)

})
