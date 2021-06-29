#' Set an observed variable
#'
#' Observed variables are those variables that contain the (quantitative)
#' observed/measured values of each unique unit (as described by the
#' \code{\link[=setIDVar]{identifying variables}}). There may be several of them
#' and in a tidy table they'd be recorded as separate columns.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param name [\code{character(1)}]\cr Name of the new measured variable.
#' @param columns [\code{integerish(.)}]\cr The column(s) in which the
#'   \emph{values} of the new variable are recorded.
#' @param top [\code{integerish(.)}]\cr In case the variable is nested in a wide
#'   identifying variable, specify here additionally the topmost row in which
#'   the variable \emph{name} sits
#' @param unit [\code{character(1)}]\cr the unit of this variable.
#' @param factor [\code{numeric(1)}]\cr the factor that needs to be multiplied
#'   with the values to convert to \code{unit}, defaults to 1. For instance, if
#'   values are recorded in acres, but shall be recorded in hectare, the factor
#'   would be 0.40468.
#' @param key [\code{character(1)}]\cr If the variable is recorded (together
#'   with other variables) so that the variable names are listed in one column
#'   and the respective values are listed in another column, give here the name
#'   of the column that contains the variable names.
#' @param value [\code{character(1)}]\cr If the variable is recorded (together
#'   with other variables) so that the variable names are listed in one column
#'   and the respective values are listed in another column, give here the level
#'   in the names column that refer to the values of this variable.
#' @param relative [\code{logical(1)}]\cr whether or not the values provided in
#'   \code{columns} and \code{rows} are relative to the cluster position(s) or
#'   whether they refer to the overall table.
#' @param distinct [\code{logical(1)}]\cr Whether or not the variable is
#'   distinct from a cluster. This is the case when the variable is recorded
#'   somewhere 'on the side' and thus not explicitly included in all clusters.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertIntegerish assertLogical
#'   assertCharacter assertNumeric testIntegerish testCharacter assert
#' @export

setObsVar <- function(schema = NULL, name = NULL, columns = NULL, top = NULL,
                      relative = FALSE, distinct = FALSE, unit = NULL,
                      factor = 1, key = NULL, value = NULL){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 2)
  assert(colInt, colList)
  rowInt <- testIntegerish(x = top, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = top, len = 2)
  assert(rowInt, rowList)
  assertLogical(x = relative, any.missing = FALSE, len = 1)
  assertLogical(x = distinct, any.missing = FALSE, len = 1)
  assertCharacter(x = unit, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertNumeric(x = factor, len = 1, any.missing = FALSE)
  # assertCharacter(x = key, len = 1, any.missing = FALSE, null.ok = TRUE)
  # assertCharacter(x = value, len = 1, any.missing = FALSE, null.ok = TRUE)

  if(is.null(schema)){
    schema <- schema_default
  }
  nClusters <- max(lengths(schema@clusters))
  if(nClusters == 0) nClusters <- 1

  # error management ----

  # ensure that 'value' is actually a value of th provided column in 'key'



  # update schema ----
  temp <- list(type = "observed",
               col = columns,
               row = top,
               rel = relative,
               dist = distinct,
               unit = unit,
               factor = factor,
               key = key,
               value = value)
  schema@variables[[name]] <- temp

  return(schema)
}