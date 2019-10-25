#' Test for a valid table
#' @param x a table to test.
#' @importFrom testthat expect_identical
#' @importFrom checkmate expect_names expect_tibble expect_list
#' @export

expect_valid_table <- function(x = NULL){

  expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 5)
  expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
  expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
  expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
  expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
  expect_identical(object = x$harvested, expected = c("1121", "1111", "1221", "1211", "2121", "2111", "2221", "2211"))
  expect_identical(object = x$production, expected = c("1122", "1112", "1222", "1212", "2122", "2112", "2222", "2212"))
}

#' Test for a valid schema
#' @param x a schema to test.
#' @importFrom testthat expect_identical
#' @importFrom checkmate expect_names expect_tibble
#' @export

expect_valid_schema <- function(x = NULL){

  expect_list(x = x, len = 2)
  expect_names(x = names(x), identical.to = c("clusters", "variables")
  )
  expect_list(x = x$clusters, len = 6)
  expect_names(x = names(x$clusters), identical.to = c("top", "left", "width", "height", "id", "header"))

  expect_list(x = x$variables, len = 5)
  expect_names(x = names(x$variables), identical.to = c("territories", "year", "commodities", "harvested", "production"))

  expect_list(x = x$variables$territories, len = 6)
  expect_names(x = names(x$variables$territories), permutation.of = c("type", "name", "split", "row", "col", "rel"))

  expect_list(x = x$variables$harvested, len = 8)
  expect_names(x = names(x$variables$harvested), permutation.of = c("type", "unit", "factor", "row", "col", "rel", "key", "value"))
}