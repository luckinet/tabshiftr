#' Set the header of a table
#'
#' The header of most tables is typically used to provide 'labels' for the data
#' in that table. With increasingly complex tables, also the header becomes
#' increasingly complex and this is especially the case in nested tables. This
#' function describes in which rows the header information are stored and how
#' they should be treated.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{integerish(.)}]\cr The rows in which the header
#'   information are stored.
#' @param relative [\code{logical(1)}]\cr Whether or not the values in
#'   \code{rows} are relative to the cluster positions or whether they refer to
#'   the overall table.
#' @param merge [\code{logical(1)}]\cr When there is more than one row, this
#'   determines whether or not those rows should be merged.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertIntegerish assertLogical
#'   testIntegerish testClass
#' @export

setHeader <- function(schema = NULL, rows = NULL, relative = FALSE,
                      merge = FALSE){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowQuo <- testClass(x = rows, classes = "quosure")
  assert(rowInt, rowQuo)
  assertLogical(x = relative, any.missing = FALSE, len = 1)
  assertLogical(x = merge, any.missing = FALSE, len = 1)

  if(is.null(schema)){
    schema <- schema_default
  }

  # error management ----

  # make some warning that when the header is relative, but this interferes with
  # the cluster specification



  # update schema ----

  if(!is.null(rows)){
    schema@header$row <- rows
  }
  if(!is.null(relative)){
    schema@header$rel <- relative
  }
  if(!is.null(merge)){
    schema@header$merge <- merge
  }

  return(schema)
}