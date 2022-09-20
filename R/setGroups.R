#' Set Groups
#'
#' This function allows to set groups for rows, columns or clusters that shall
#' be aggregated/summarised.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{integerish(.)}]\cr .
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass testIntegerish testClass
#' @export

setGroups <- function(schema = NULL, rows = NULL, columns = NULL,
                      clusters = NULL){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows)
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns)
  clustInt <- testIntegerish(x = clusters, lower = 1, min.len = 1, null.ok = TRUE)
  clustList <- testList(x = clusters)
  assert(rowInt, rowList)
  assert(colInt, colList)
  assert(clustInt, clustList)

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(rows)){
    rws <- list()
    if(rowInt){
      rws$fun <- "id"
      rws$ind <- rows
    }
    if(rowList){
      rws$fun <- rows$by
      rws$ind <- rows$groups
    }
    schema@groups$rows <- rws
  }

  # if(!is.null(columns)){
  #   schema@groups$cols <- columns
  # }
  #
  # if(!is.null(clusters)){
  #   schema@groups$clusters <- clusters
  # }

  return(schema)

}