library(tabshiftr)
library(testthat)
library(checkmate)
context("mismatch")


test_that("split a column that contains several identifying variables in one column", {

  input <- tabs2shift$merged_column

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2, split = ".+?(?=_)") %>%
    setIDVar(name = "commodities", columns = 2, split = "(?<=\\_).*") %>%
    setObsVar(name = "harvested", columns = 4) %>%
    setObsVar(name = "production", columns = 5)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


test_that("recognise an identifying variable that is actually a merge of two columns", {

  input <- tabs2shift$split_column

  schema <-
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = c(2, 4), merge = " ") %>%
    setIDVar(name = "commodities", columns = 5) %>%
    setObsVar(name = "harvested", columns = 6) %>%
    setObsVar(name = "production", columns = 7)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that("recognise a distinct variable that is not available for every cluster", {

  input <- tabs2shift$clusters_messy

  schema <- setCluster(id = "territories",
                       left = c(1, 1, 4), top = c(1, 8, 8)) %>%
    setIDVar(name = "territories", columns = c(1, 1, 4), rows = c(2, 9, 9)) %>%
    setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
    setIDVar(name = "commodities", columns = c(1, 1, 4)) %>%
    setObsVar(name = "harvested", columns = c(2, 2, 5)) %>%
    setObsVar(name = "production", columns = c(3, 3, 6))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 3)

})

