library(testthat)
library(checkmate)
context("reorganise")


test_that("a schema is printed properly", {
  schema <- makeSchema(
    list(clusters =
           list(row = c(3, 13), col = 2, height = 8, id = "measured"),
         header = list(row = 1),
         variables =
           list(territories =
                  list(type = "id", col = 2),
                year =
                  list(type = "id", col = 3),
                commodities =
                  list(type = "id", col = 4),
                harvested =
                  list(type = "measured", unit = "ha", factor = 1, col = 5,
                       key = "cluster", value = 1),
                production =
                  list(type = "measured", unit = "t", factor = 1, col = 5,
                       key = "cluster", value = 2))))

  output <- capture.output(schema)
  expect_character(x = output, len = 11)
  expect_true(output[2] == "    origin: 3|2, 13|2  (row|col)")
  expect_true(output[5] == "   variable      type       col   key       value   rel   dist ")

})
