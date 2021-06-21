library(tabshiftr)
library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("tidy")


test_that("already tidy table", {

  input <- read_csv(paste0(system.file("test_datasets",
                                       package = "tabshiftr",
                                       mustWork = TRUE), "/tidy_table.csv"),
                    col_names = FALSE)

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})
