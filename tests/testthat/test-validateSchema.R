library(tabshiftr)
library(testthat)
library(checkmate)
context("validateSchema")


test_that("relative positions in clusters", {

  # vertical clusters
  input <- tabs2shift$clusters_vertical

  schema <- setCluster(id = "territories",
                       left = 1, top = c(3, 9)) %>%
    setIDVar(name = "territories", columns = 1, rows = 1, relative = TRUE) %>%
    setIDVar(name = "year", columns = 2, relative = TRUE) %>%
    setIDVar(name = "commodities", columns = 5, relative = TRUE) %>%
    setObsVar(name = "harvested", columns = 6, relative = TRUE) %>%
    setObsVar(name = "production", columns = 7, relative = TRUE)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)


  # horizontal cluster
  input <- tabs2shift$clusters_horizontal

  schema <- setCluster(id = "territories",
                       left = c(1, 6), top = 2) %>%
    setIDVar(name = "territories", columns = 1, rows = 1, relative = TRUE) %>%
    setIDVar(name = "year", columns = 2, relative = TRUE) %>%
    setIDVar(name = "commodities", columns = 1, relative = TRUE) %>%
    setObsVar(name = "harvested", columns = 3, relative = TRUE) %>%
    setObsVar(name = "production", columns = 4, relative = TRUE)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)


  # several clusters
  input <- tabs2shift$clusters_messy

  schema <- setCluster(id = "territories",
                       left = c(1, 1, 4), top = c(1, 8, 8)) %>%
    setIDVar(name = "territories", columns = 1, rows = 2, relative = TRUE) %>%
    setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
    setIDVar(name = "commodities", columns = 1, relative = TRUE) %>%
    setObsVar(name = "harvested", columns = 2, relative = TRUE) %>%
    setObsVar(name = "production", columns = 3, relative = TRUE)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 3)

})

