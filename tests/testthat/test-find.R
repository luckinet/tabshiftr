library(tabshiftr)
library(testthat)
library(checkmate)
context(".find")


test_that(".find columns and rows based on regular expressions", {

  input <- tabs2shift$two_wide_id

  schema <-
    setIDVar(name = "territories", columns = .find("unit*")) %>%
    setIDVar(name = "year", columns = .find("year*"), rows = .find("year*")) %>%
    setIDVar(name = "commodities", columns = .find("soy|mai*"), rows = .find("soy|mai*")) %>%
    setObsVar(name = "harvested", columns = .find("harv*"), top = .find("harv*")) %>%
    setObsVar(name = "production", columns = .find("prod*"), top = .find("prod*"))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that(".find columns and rows in listed and wide table", {

  input <- tabs2shift$listed_column_two_wide

  schema <-
    setIDVar(name = "territories", columns = .find("unit*")) %>%
    setIDVar(name = "year", columns = .find(is.character, row = 2), rows = .find("year*")) %>%
    setIDVar(name = "commodities", columns = .find("soy|mai*"), rows = .find("soy|mai*")) %>%
    setObsVar(name = "harvested", columns = .find(is.character, row = 3),
              key = 3, value = "harvested") %>%
    setObsVar(name = "production", columns = .find(is.character, row = 3),
              key = 3, value = "production")

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})

test_that(".find cluster positions based on regular expressions", {

  input <- tabs2shift$clusters_messy

  schema <- setCluster(id = "territories",
                       left = .find("comm*"), top = .find("comm*")) %>%
    setIDVar(name = "territories", columns = c(1, 1, 4), rows = c(2, 9, 9)) %>%
    setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
    setIDVar(name = "commodities", columns = c(1, 1, 4)) %>%
    setObsVar(name = "harvested", columns = c(2, 2, 5)) %>%
    setObsVar(name = "production", columns = c(3, 3, 6))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 3)

})

test_that(".find filter rows based on a function", {

  input <- tabs2shift$messy_rows

  schema <-
    setFilter(rows = .find(by = is.numeric, col = 1), invert = TRUE) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})


