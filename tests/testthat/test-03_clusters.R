library(tabshiftr)
library(testthat)
library(checkmate)
context("clusters")

test_that("regular vertical clusters of otherwise tidy data", {

  input <- tabs2shift$clusters_vertical

  schema <- setCluster(id = "territories",
                       left = 1, top = c(3, 9)) %>%
    setIDVar(name = "territories", columns = 1, rows = c(3, 9)) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 5) %>%
    setObsVar(name = "harvested", columns = 6) %>%
    setObsVar(name = "production", columns = 7)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("regular horizontal clusters of otherwise tidy data", {

  input <- tabs2shift$clusters_horizontal

  schema <- setCluster(id = "territories",
                       left = c(1, 6), top = 2) %>%
    setIDVar(name = "territories", columns = c(1, 6), rows = 2) %>%
    setIDVar(name = "year", columns = c(2, 7)) %>%
    setIDVar(name = "commodities", columns = c(1, 6)) %>%
    setObsVar(name = "harvested", columns = c(3, 8)) %>%
    setObsVar(name = "production", columns = c(4, 9))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("clusters that are aggregated per observed variable", {

  input <- tabs2shift$clusters_observed

  schema <- setCluster(id = "observed",
                       left = 1, top = c(2, 12)) %>%
    setIDVar(name = "territories", columns = 2) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = 5) %>%
    setObsVar(name = "harvested", columns = 7, key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = 7, key = "cluster", value = 2)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("clusters that are nested into groups", {

  input <- tabs2shift$clusters_nested

  schema <- setCluster(id = "sublevel",
                       group = "territories", member = c(1, 1, 2),
                       left = 1, top = c(3, 8, 15)) %>%
    setIDVar(name = "territories", columns = 1, rows = c(2, 14)) %>%
    setIDVar(name = "sublevel", columns = 1, rows = c(3, 8, 15)) %>%
    setIDVar(name = "year", columns = 7) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 3, groups = TRUE)

})

