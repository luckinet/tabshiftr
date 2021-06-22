library(tabshiftr)
library(testthat)
library(checkmate)
context("show")


test_that("a schema is printed properly", {

  schema <- setCluster(id = "observed", left = 2, top = c(3, 12), height = 8) %>%
    setIDVar(name = "territories", columns = 2) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = 4) %>%
    setObsVar(name = "harvested", columns = 5, unit = "ha",
              key = "cluster", value = 1) %>%
    setObsVar(name = "production", columns = 5, unit = "t",
              key = "cluster", value = 2)

  output <- capture.output(schema)
  expect_character(x = output, len = 11)
  expect_true(output[2] == "    origin : 3|2, 12|2  (row|col)")
  expect_true(output[5] == "   variable      type       col   key       value    ")

})
