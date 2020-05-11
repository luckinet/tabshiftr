#' Test for a valid table
#'
#' This function is a collection of expectations which ensure that the output of
#' \code{\link{reorganise}} is formally and contentwise correct. It is used in
#' the tests of this package.
#' @param x a table to test.
#' @param units the number of units in the output table (from 1 to 3)
#' @return Either an error message of the invalid expectations, or the output of
#'   the last successful expectation.
#' @importFrom testthat expect_identical
#' @importFrom checkmate expect_names expect_tibble expect_list assertChoice

expect_valid_table <- function(x = NULL, units = 1){

  assertChoice(x = units, choices = c(1:3))

  if(units == 1){

    expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 5)
    expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
    expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1"))
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean"))
    expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211))
    expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212))

  } else if(units == 2){

    expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 5)
    expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
    expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
    expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211))
    expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212))

  } else if(units == 3){

    expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 5)
    expect_names(x = colnames(x), permutation.of = c("territories", "year", "commodities", "harvested", "production") )
    expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2", "unit 3", "unit 3", "unit 3", "unit 3"))
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
    expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211, 3121, 3111, 3221, 3211))
    expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212, 3122, 3112, 3222, 3212))

  }
}