#' Set filters
#'
#' This function allows to specify additional rules to filter certain rows
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{integerish(.)}]\cr the rows to ignore.
#' @param invert [\code{logical(1)}]\cr whether or not to invert the values
#'   provided in \code{columns} and \code{rows}.
#' @details Currently the functionality is restricted to masking out columns and
#'   rows globally across the whole table.
#' @importFrom checkmate assertClass testIntegerish testClass
#' @export

setFilter <- function(schema = NULL, rows = NULL, invert = FALSE){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowQuo <- testClass(x = rows, classes = "quosure")
  assert(rowInt, rowQuo)

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(rows)){
    schema@filter$row <- rows
  }

  schema@filter$invert <- invert

  return(schema)

}