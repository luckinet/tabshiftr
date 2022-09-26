library(tabshiftr)
library(testthat)
library(checkmate)
context("setGroups")

test_that("groups of rows", {

  input <- tabs2shift$group_simple

  schema <-
    setGroups(rows = .sum(c(1, 2))) %>%
    setGroups(rows = .sum(c(4, 5))) %>%
    setGroups(rows = .sum(c(7, 8))) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3:6), rows = 3) %>%
    setObsVar(name = "harvested", columns = c(3, 4)) %>%
    setObsVar(name = "production", columns = c(5, 6))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

# test_that("groups of columns", {
#
#
# })

test_that("apply function to summarise merged rows", {

  input <- tabs2shift$group_sum

  schema <-
    setGroups(rows = .sum(c(3, 4))) %>%
    setGroups(rows = .sum(c(6, 7))) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(3:6), rows = 2) %>%
    setObsVar(name = "harvested", columns = c(3, 4)) %>%
    setObsVar(name = "production", columns = c(5, 6))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

