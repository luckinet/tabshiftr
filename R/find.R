#' Determine row or column on the fly
#'
#' Find the location of a variable not based on it's columns/rows, but based on
#' a regular expression or function
#' @param by [\code{character(1)}]\cr character string containing a regular
#'   expression or function to identify columns or rows in the input table on
#'   the fly.
#' @param col [\code{integerish(1)}]\cr optionally, in case \code{by} should not
#'   be applied to the whole table, this is the column in which to apply
#'   \code{by}.
#' @details This functions is basically a wild-card for when columns or rows are
#'   not known ad-hoc, but have to be assigned on the fly. This can be very
#'   helpful when several tables contain the same variables, but the arrangement
#'   may be slightly different.
#' @section How does this work: The first step in using any schema is validating
#'   it via the function \code{\link{validateSchema}}. This happens by default
#'   in \code{\link{reorganise}}, but can also be done manually, for example
#'   when debugging complicated schema descriptions.
#'
#'   In case that function encounters a schema that wants to find columns or
#'   rows on the fly via \code{.find}, it combines all cells of columns and all
#'   cells of rows into one character string and matches the regular expression
#'   or function on those. Columns/rows that have a match are returned as the
#'   respective column/row value.
#' @return the index values where the target was found.
#' @examples
#' # use regular expressions to find cell positions
#' (input <- tabs2shift$clusters_messy)
#'
#' schema <- setCluster(id = "territories",
#'                      left = .find("comm*"), top = .find("comm*")) %>%
#'   setIDVar(name = "territories", columns = c(1, 1, 4), rows = c(2, 9, 9)) %>%
#'   setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
#'   setIDVar(name = "commodities", columns = c(1, 1, 4)) %>%
#'   setObsVar(name = "harvested", columns = c(2, 2, 5)) %>%
#'   setObsVar(name = "production", columns = c(3, 3, 6))
#'
#' schema
#' validateSchema(schema = schema, input = input)
#'
#' # use a function to find rows
#' (input <- tabs2shift$messy_rows)
#'
#' schema <-
#'   setFilter(rows = .find(by = is.numeric, col = 1), invert = TRUE) %>%
#'   setIDVar(name = "territories", columns = 1) %>%
#'   setIDVar(name = "year", columns = 2) %>%
#'   setIDVar(name = "commodities", columns = 3) %>%
#'   setObsVar(name = "harvested", columns = 5) %>%
#'   setObsVar(name = "production", columns = 6)
#'
#' reorganise(schema = schema, input = input)
#' @importFrom checkmate testCharacter testFunction assert
#' @importFrom purrr map_chr
#' @importFrom rlang enquo
#' @export

.find <- function(by, col = NULL){

  isPat <- testCharacter(x = by, min.len = 1, any.missing = FALSE)
  isFun <- testFunction(x = by)
  assert(isPat, isFun)

  temp <- enquo(by)

  out <- list(by = temp, col = col)

  return(out)

}