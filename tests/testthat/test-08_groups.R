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

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

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

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that("apply function to merge wide variables", {

  input <- tabs2shift$one_wide_id_sum

  schema <-
    setGroups(rows = .sum(c(1, 2), character = function(x) paste0(na.omit(x), collapse = ""))) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = c(4, 6), rows = 2) %>%
    setObsVar(name = "harvested", columns = c(4, 6), top = 3) %>%
    setObsVar(name = "production", columns = c(5, 7), top = 3)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})


