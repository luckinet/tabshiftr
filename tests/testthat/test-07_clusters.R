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

test_that("regular vertical clusters with a listed observed variable", {

  input <- tabs2shift$listed_column_wide

  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("regular vertical clusters with a listed observed variable and an implicit variable", {

  input <- tabs2shift$listed_column_wide

  # territories absolute
  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setIDVar(name = "region", value = "group 1") %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2, groups = TRUE)

  # territories relative
  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setIDVar(name = "region", value = "group 1") %>%
    setIDVar(name = "territories", columns = 1, rows = 1, relative = T) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")



  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2, groups = TRUE)

  # territories implicit
  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setFilter(rows = c(1, 4, 5, 9, 10)) %>%
    setIDVar(name = "territories", value = "unit 1") %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  out <- reorganise(input = input, schema = schema)

  expect_equal(out$territories, c("unit 1", "unit 1", "unit 1", "unit 1"))
  expect_equal(out$year, c("year 2", "year 2", "year 2", "year 2"))
  expect_equal(out$commodities, c("maize", "maize", "soybean", "soybean"))
  expect_equal(out$harvested, c(1221, 2221, 1211, 2211))
  expect_equal(out$production, c(1222, 2222, 1212, 2212))

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

  schema <- setCluster(id = "territories",
                       group = "region", member = c(1, 1, 2),
                       left = 1, top = c(3, 8, 15)) %>%
    setIDVar(name = "region", columns = 1, rows = c(2, 14)) %>%
    setIDVar(name = "territories", columns = 1, rows = c(3, 8, 15)) %>%
    setIDVar(name = "year", columns = 7) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 3, groups = TRUE)

})

