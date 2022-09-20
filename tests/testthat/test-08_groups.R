library(tabshiftr)
library(testthat)
library(checkmate)
context("setGroups")

test_that("groups of rows", {

  input <- tabs2shift$group_simple

  schema <-
    # setGroups(rows = c(1, 1, 2, 3, 3, 4, 5, 5, 6)) %>%
    setGroups(rows = .group(fn = function(x) paste0(na.omit(x), collapse = " "), c(1, 2))) %>%
    setGroups(rows = .group(fn = function(x) paste0(na.omit(x), collapse = " "), c(4, 5))) %>%
    setGroups(rows = .group(fn = function(x) paste0(na.omit(x), collapse = " "), c(7, 8))) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3:6), rows = 3) %>%
    setObsVar(name = "harvested", columns = c(3, 4)) %>%
    setObsVar(name = "production", columns = c(5, 6))
  #
  # .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

# test_that("groups of columns", {
#
#
# })

test_that("apply function to summarise merged rows", {

  # input <- tabs2shift$group_sum
  #
  # schema <-
  #   # setGroups(rows = .group(fn = sum, c(1, 2, 3, 3, 4, 5, 5, 6))) %>%
  #   setGroups(rows = .group(fn = sum, c(3, 4), c(6, 7))) %>%
  #   setIDVar(name = "territories", columns = 1) %>%
  #   setIDVar(name = "year", columns = 2) %>%
  #   setIDVar(name = "commodities", columns = c(3:6), rows = 2) %>%
  #   setObsVar(name = "harvested", columns = c(3, 4)) %>%
  #   setObsVar(name = "production", columns = c(5, 6))
  #
  # .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("clusters that are nested into groups", {
#
#   input <- tabs2shift$clusters_nested
#
#   schema <- setCluster(id = "territories", group = "region",
#                        left = 1, top = c(3, 8, 15)) %>%
#     # setGroups(clusters = c(1, 1, 2)) %>%
#     setIDVar(name = "region", columns = 1, rows = c(2, 14)) %>%
#     setIDVar(name = "territories", columns = 1, rows = c(3, 8, 15)) %>%
#     setIDVar(name = "year", columns = 7) %>%
#     setIDVar(name = "commodities", columns = 2) %>%
#     setObsVar(name = "harvested", columns = 5) %>%
#     setObsVar(name = "production", columns = 6)
#
#   .expect_valid_table(x = reorganise(input = input, schema = schema), units = 3, groups = TRUE)

})

