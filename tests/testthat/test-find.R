library(tabshiftr)
library(testthat)
library(checkmate)
context(".find")


test_that(".find columns and rows based on regular expressions", {

  input <- tabs2shift$two_wide_id

  schema <-
    setIDVar(name = "territories", columns = .find(pattern = "unit*")) %>%
    setIDVar(name = "year", columns = .find(pattern = "year*"), rows = .find(pattern = "year*")) %>%
    setIDVar(name = "commodities", columns = .find(pattern = "soy|mai*"), rows = .find(pattern = "soy|mai*")) %>%
    setObsVar(name = "harvested", columns = .find(pattern = "harv*"), top = .find(pattern = "harv*")) %>%
    setObsVar(name = "production", columns = .find(pattern = "prod*"), top = .find(pattern = "prod*"))

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that(".find columns and rows in listed and wide table", {

  input <- tabs2shift$listed_column_two_wide

  schema <-
    setIDVar(name = "territories", columns = .find(pattern = "unit*")) %>%
    setIDVar(name = "year", columns = .find(fun = is.character, row = 2), rows = .find(pattern = "year*")) %>%
    setIDVar(name = "commodities", columns = .find(pattern = "soy|mai*"), rows = .find(pattern = "soy|mai*")) %>%
    setObsVar(name = "harvested", columns = .find(fun = is.character, row = 3),
              key = 3, value = "harvested") %>%
    setObsVar(name = "production", columns = .find(fun = is.character, row = 3),
              key = 3, value = "production")

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that(".find cluster positions based on regular expressions", {

  input <- tabs2shift$clusters_messy

  schema <- setCluster(id = "territories",
                       left = .find(pattern = "comm*"), top = .find(pattern = "comm*")) %>%
    setIDVar(name = "territories", columns = c(1, 1, 4), rows = c(2, 9, 9)) %>%
    setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
    setIDVar(name = "commodities", columns = c(1, 1, 4)) %>%
    setObsVar(name = "harvested", columns = c(2, 2, 5)) %>%
    setObsVar(name = "production", columns = c(3, 3, 6))

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 3)

})

test_that(".find filter-rows based on a function", {

  input <- tabs2shift$messy_rows

  schema <-
    setFilter(rows = .find(fun = is.numeric, col = 1, invert = TRUE)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})

test_that(".find filter-rows based on several character strings in one column", {

  input <- tabs2shift$messy_rows

  schema <-
    setFilter(rows = .find(pattern = c("all", "none", "xyz", "5"), col = 3, invert = TRUE)) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)

})


test_that("relative positions in clusters", {

  # vertical clusters
  input <- tabs2shift$clusters_vertical

  schema <- setCluster(id = "territories",
                       left = 1, top = c(3, 9)) %>%
    setIDVar(name = "territories", columns = 1, rows = .find(row = 1, relative = TRUE)) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 5) %>%
    setObsVar(name = "harvested", columns = 6) %>%
    setObsVar(name = "production", columns = 7)

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)


  # horizontal cluster
  input <- tabs2shift$clusters_horizontal

  schema <- setCluster(id = "territories",
                       left = c(1, 6), top = 2) %>%
    setIDVar(name = "territories", columns = .find(col = 1, relative = TRUE), rows = .find(row = 1, relative = TRUE)) %>%
    setIDVar(name = "year", columns = .find(col = 2, relative = TRUE)) %>%
    setIDVar(name = "commodities", columns = .find(col = 1, relative = TRUE)) %>%
    setObsVar(name = "harvested", columns = .find(col = 3, relative = TRUE)) %>%
    setObsVar(name = "production", columns = .find(col = 4, relative = TRUE))

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2)


  # several clusters
  input <- tabs2shift$clusters_messy

  schema <- setCluster(id = "territories",
                       left = c(1, 1, 4), top = c(1, 8, 8)) %>%
    setIDVar(name = "territories", columns = .find(col = 1, relative = TRUE), rows = .find(row = 2, relative = TRUE)) %>%
    setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
    setIDVar(name = "commodities", columns = .find(col = 1, relative = TRUE)) %>%
    setObsVar(name = "harvested", columns = .find(col = 2, relative = TRUE)) %>%
    setObsVar(name = "production", columns = .find(col = 3, relative = TRUE))

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 3)

})


test_that("vertical clusters with a listed observed variable and an implicit variable", {

  input <- tabs2shift$listed_column_wide

  # territories relative
  schema <- setCluster(id = "territories",
                       left = 1, top = c(2, 7)) %>%
    setIDVar(name = "region", value = "group 1") %>%
    setIDVar(name = "territories", columns = 1, rows = .find(row = 1, relative = TRUE)) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = c(6, 7), rows = 1) %>%
    setObsVar(name = "harvested", columns = c(6, 7),
              key = 4, value = "harvested") %>%
    setObsVar(name = "production", columns = c(6, 7),
              key = 4, value = "production")

  reorganise(input = input, schema = schema) %>%
    arrange(territories, year, commodities) %>%
    .expect_valid_table(units = 2, groups = TRUE)

})


test_that("vertical cluster with a wide identifying variable that has uneven number of wide columns", {

  input <- tabs2shift$clusters_one_wide_uneven

  schema <- setCluster(id = "year", left = 1, top = c(2, 5)) %>%
    setIDVar(name = "territories", columns = c(4:6), rows = .find(pattern = "unit", row = 1, relative = TRUE)) %>%
    setIDVar(name = "year", columns = 1) %>%
    setIDVar(name = "commodities", columns = 2) %>%
    setObsVar(name = "harvested", columns = c(4:6))

  out <- reorganise(input = input, schema = schema)

  expect_equal(out$territories, c("unit 2", "unit 3", "unit 2", "unit 3", "unit 1", "unit 2", "unit 3", "unit 1", "unit 2", "unit 3"))
  expect_equal(out$year, c("year 1", "year 1", "year 1", "year 1", "year 2", "year 2", "year 2", "year 2", "year 2", "year 2"))
  expect_equal(out$commodities, c("soybean", "soybean", "maize", "maize", "soybean", "soybean", "soybean", "maize", "maize", "maize"))
  expect_equal(out$harvested, c(2111, 3111, 2121, 3121, 1211, 2211, 3211, 1221, 2221, 3221))

})
