#' Set Groups
#'
#' This function allows to set groups for rows, columns or clusters that shall
#' be summarised.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{list(3)}]\cr the output of \code{\link{.sum}} indicating
#'   the rows and a function according to which those rows should be summarised.
#' @param columns [\code{list(3)}]\cr the output of \code{\link{.sum}}
#'   indicating the columns and a function according to which those columns
#'   should be summarised.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertList
#' @export

setGroups <- function(schema = NULL, rows = NULL, columns = NULL){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  # rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  # rowList <- testList(x = rows)
  assertList(x = rows, len = 1, null.ok = TRUE)
  # assert(rowInt, rowList)
  # colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  # colList <- testList(x = columns)
  assertList(x = columns, len = 1, null.ok = TRUE)
  # assert(colInt, colList)
  # clustInt <- testIntegerish(x = clusters, lower = 1, min.len = 1, null.ok = TRUE)
  # clustList <- testList(x = clusters)
  # assert(clustInt, clustList)

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(rows)){
    schema@groups$rows <- c(schema@groups$rows, rows)
  }

  if(!is.null(columns)){
    schema@groups$cols <- c(schema@groups$cols, columns)
  }

  # if(!is.null(clusters)){
  #   schema@groups$clusters <- c(schema@groups$clusters, clusters)
  # }

  return(schema)

}