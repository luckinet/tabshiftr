#' Set filters
#'
#' This function allows to specify additional rules to filter certain rows
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{integerish(.)}]\cr rows that are mentioned here are kept.
#' @param columns [\code{integerish(.)}]\cr columns that are mentioned here are
#'   kept.
#' @param operator [\code{function(1)}]\cr \code{\link[base]{Logic}} operators
#'   by which the current filter should be combined with the directly preceeding
#'   filter; hence this argument is not used in case no other filter was defined
#'   before it.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' (input <- tabs2shift$messy_rows)
#'
#' # select rows where there is 'unit 2' in column 1 or 'year 2' in column 2
#' schema <-
#'   setFilter(rows = .find(by = "unit 2", col = 1)) %>%
#'   setFilter(rows = .find(by = "year 2", col = 2), operator = `|`) %>%
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

setFilter <- function(schema = NULL, rows = NULL, columns = NULL,
                      operator = NULL){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows, len = 1)
  assert(rowInt, rowList)
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 1)
  assert(colInt, colList)
  if(rowList) assertSubset(x = names(rows), choices = c("find"))
  if(colList) assertSubset(x = names(columns), choices = c("find"))

  if(is.null(schema)){
    schema <- schema_default
  }

  if(is.null(operator)){
    operator <- `&`
  }

  if(!is.null(rows)){
    if(!is.list(rows)){
      rows <- list(position = rows)
    }
    if(!is.null(schema@filter$row)){
      rows <- c(operator = operator, rows)
    }
    schema@filter$row <- c(schema@filter$row, rows)
  }

  if(!is.null(columns)){
    if(!is.list(columns)){
      columns <- list(position = columns)
    }
    if(!is.null(schema@filter$row)){
      columns <- c(operator = operator, columns)
    }
    schema@filter$col <- c(schema@filter$col, columns)
  }

  return(schema)

}