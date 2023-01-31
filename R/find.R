#' Determine row or column on the fly
#'
#' Find the location of a variable not based on it's columns/rows, but based on
#' a regular expression or function
#' @param fun [\code{character(1)}]\cr function to identify columns or rows in
#'   the input table on the fly.
#' @param pattern [\code{character(1)}]\cr character string containing a regular
#'   expression to identify columns or rows in the input table on the fly.
#' @param col [\code{integerish(1)}]\cr optionally, in case this function should
#'   only be applied to certain columns, provides this here.
#' @param row [\code{integerish(1)}]\cr optionally, in case this function should
#'   only be applied to certain rows, provides this here.
#' @param invert [\code{logical(1)}]\cr whether or not the identified columns or
#'   rows should be inverted, i.e., all other columns or rows should be
#'   selected.
#' @param relative [\code{logical(1)}]\cr whether or not the values provided in
#'   \code{col} or \code{row} are relative to the cluster position(s) or whether
#'   they are absolute positions, i.e, refer to the overall table.
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
#'                      left = .find(pattern = "comm*"), top = .find(pattern = "comm*")) %>%
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
#'   setFilter(rows = .find(fun = is.numeric, col = 1, invert = TRUE)) %>%
#'   setIDVar(name = "territories", columns = 1) %>%
#'   setIDVar(name = "year", columns = 2) %>%
#'   setIDVar(name = "commodities", columns = 3) %>%
#'   setObsVar(name = "harvested", columns = 5) %>%
#'   setObsVar(name = "production", columns = 6)
#'
#' reorganise(schema = schema, input = input)
#' @importFrom checkmate testCharacter testFunction assert assertLogical
#' @importFrom purrr map_chr
#' @importFrom rlang enquo
#' @export

.find <- function(fun = NULL, pattern = NULL, col = NULL, row = NULL,
                  invert = FALSE, relative = FALSE){

  assertFunction(x = fun, null.ok = TRUE)
  assertCharacter(x = pattern, null.ok = TRUE)
  assertLogical(x = invert, len = 1)
  assertLogical(x = relative, len = 1)

  if(!is.null(fun) & !is.null(pattern)){
    stop("please specifiy either 'fun' or 'pattern', but not both.")
  }

  if(!is.null(fun)){
    temp <- enquo(fun)
  } else if(!is.null(pattern)){
    temp <- enquo(pattern)
  } else {
    temp <- NULL
  }

  out <- list(find = list(by = temp, col = col, row = row, invert = invert, relative = relative))

  return(out)

}