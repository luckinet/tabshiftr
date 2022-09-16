library(tabshiftr)
library(testthat)
library(checkmate)
context("setFormat")


test_that("extract flags into their own column", {

  input <- tabs2shift$tidy
  input$X5[3] <- paste0(input$X5[3], "p")
  input$X6[7] <- paste0(input$X6[7], " $")

  flags <- tibble(flag = c("p", "$"),
                  value = c("provisional", "only applicable in case of X"))

  schema <-
    setFormat(flags = flags) %>%
    setIDVar(name = "territories", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", columns = 5) %>%
    setObsVar(name = "production", columns = 6)

  .expect_valid_table(x = reorganise(input = input, schema = schema), units = 2, flags = TRUE)

})



