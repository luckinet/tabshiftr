library(tabshiftr)
library(testthat)
library(checkmate)
context(".find")


test_that(".find columns based on regular expressions", {

  input <- tabs2shift$two_wide_id

  schema <-
    setIDVar(name = "territories", columns = .find("unit*")) %>%
    setIDVar(name = "year", columns = .find("year*"), rows = .find("year*")) %>%
    setIDVar(name = "commodities", columns = .find("soy|mai*"), rows = .find("soy|mai*")) %>%
    setObsVar(name = "harvested", columns = .find("harv*"), top = .find("harv*")) %>%
    setObsVar(name = "production", columns = .find("prod*"), top = .find("prod*"))

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2)

})
