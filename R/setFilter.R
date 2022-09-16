#' Set filters
#'
#' This function allows to specify additional rules to filter certain rows
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{integerish(.)}]\cr rows that are mentioned here are kept.
#' @param invert [\code{logical(1)}]\cr whether or not to invert the values
#'   provided in \code{columns} and \code{rows}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' (input <- tabs2shift$messy_rows)
#'
#' # select only 'unit 2' and 'year 2'
#' schema <-
#'   setFilter(rows = .find(by = "unit 2", col = 1)) %>%
#'   setFilter(rows = .find(by = "year 2", col = 2)) %>%
#'   setIDVar(name = "territories", columns = 1) %>%
#'   setIDVar(name = "year", columns = 2) %>%
#'   setIDVar(name = "commodities", columns = 3) %>%
#'   setObsVar(name = "harvested", columns = 5) %>%
#'   setObsVar(name = "production", columns = 6)
#'
#' reorganise(schema = schema, input = input)
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass testIntegerish testClass
#' @export

setFilter <- function(schema = NULL, rows = NULL, invert = FALSE){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows, len = 3)
  assert(rowInt, rowList)

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(rows)){
    schema@filter$row <- c(schema@filter$row, list(rows))
    # schema@filter$row <- rows
  }

  schema@filter$invert <- invert

  # need a test that ensures header rows are not filtered out

  return(schema)

}