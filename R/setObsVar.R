#' Set an observed variable
#'
#' Observed variables are those variables that contain the (quantitative)
#' observed/measured values of each unique unit (as described by the
#' \code{\link[=setIDVar]{identifying variables}}). There may be several of them
#' and in a tidy table they'd be recorded as separate columns.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param name [\code{character(1)}]\cr Name of the new observed variable.
#' @param type [\code{character(1)}]\cr data type of the new observed
#'   variable. Possible values are \code{"c/character"}, \code{"i/integer"},
#'   \code{"n/numeric"}, \code{"l/logical"}, \code{"D/date"} or \code{"_/skip"}.
#' @param columns [\code{integerish(.)}]\cr The column(s) in which the
#'   \emph{values} of the new variable are recorded.
#' @param top [\code{integerish(.)}]\cr In case the variable is nested in a wide
#'   identifying variable, specify here additionally the topmost row in which
#'   the variable \emph{name} sits.
#' @param factor [\code{numeric(1)}]\cr the factor that needs to be multiplied
#'   with the values to convert to the target unit, defaults to 1. For instance,
#'   if values are recorded in acres, but shall be recorded in hectare, the
#'   factor would be 0.40468.
#' @param key [\code{integerish(1)}]\cr If the variable is recorded (together
#'   with other variables) so that the variable names are listed in one column
#'   and the respective values are listed in another column, give here the
#'   number of the column that contains the variable names. Can alternatively be
#'   "cluster", in case observed variables are the cluster ID.
#' @param value [\code{character(1)}]\cr If the variable is recorded (together
#'   with other variables) so that the variable names are listed in one column
#'   and the respective values are listed in another column, give here the level
#'   in the names column that refer to the values of this variable.
#' @param distinct [\code{logical(1)}]\cr Whether or not the variable is
#'   distinct from a cluster. This is the case when the variable is recorded
#'   somewhere 'on the side' and thus not explicitly included in all clusters.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertIntegerish assertLogical assertSubset
#'   assertCharacter assertNumeric testIntegerish testCharacter assert
#' @importFrom dplyr case_when
#' @export

setObsVar <- function(schema = NULL, name = NULL, type = "numeric",
                      columns = NULL, top = NULL, distinct = FALSE, factor = 1,
                      key = NULL, value = NULL){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 1)
  assert(colInt, colList)
  if(colList) assertSubset(x = names(columns), choices = c("find"))
  rowInt <- testIntegerish(x = top, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = top, len = 1)
  assert(rowInt, rowList)
  if(rowList) assertSubset(x = names(top), choices = c("find"))
  assertLogical(x = distinct, any.missing = FALSE, len = 1)
  assertNumeric(x = factor, len = 1, any.missing = FALSE)
  if(is.character(key)){
    assertSubset(x = key, choices = "cluster", empty.ok = FALSE)
  }

  data_type <- case_when(
    type %in% c("i", "integer") ~ "integer",
    type %in% c("n", "numeric") ~ "numeric",
    type %in% c("l", "logical") ~ "logical",
    type %in% c("D", "Date") ~ "Date",
    type %in% c("_", "skip") ~ "skip",
    .default = "character"
  )

  if(is.null(schema)){
    schema <- schema_default
  }

  # update schema ----
  temp <- list(vartype = "observed",
               datype = data_type,
               col = columns,
               row = top,
               dist = distinct,
               factor = factor,
               key = key,
               value = value)
  schema@variables[[name]] <- temp

  # test for problems ----
  .reportProblems(schema = schema)

  return(schema)
}