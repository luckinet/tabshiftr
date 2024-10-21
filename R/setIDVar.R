#' Set an identifying variable
#'
#' Identifying variables are those variables that describe the (qualitative)
#' properties that make each observation (as described by the
#' \code{\link[=setObsVar]{observed variables}}) unique.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param name [\code{character(1)}]\cr Name of the new identifying variable.
#' @param type [\code{character(1)}]\cr data type of the new identifying
#'   variable. Possible values are \code{"c/character"}, \code{"i/integer"},
#'   \code{"n/numeric"}, \code{"l/logical"}, \code{"D/Date"} or \code{"_/skip"}.
#'   For \code{"D/Date"}, the value has to follow the form \code{YYYY-MM-DD},
#'   where dates that don't match that are replaced by NA.
#' @param value [\code{character(1)}]\cr In case the variable is an implicit
#'   variable (i.e., which is not in the origin table), specify it here.
#' @param columns [\code{integerish(.)}]\cr The column(s) in which the
#'   \emph{values} of the new variable are recorded.
#' @param rows [\code{integerish(.)}]\cr In case the variable is in several
#'   columns, specify here additionally the row in which the \emph{names} are
#'   recorded.
#' @param split [\code{character(1)}]\cr In case the variable is part of a
#'   compound value, this should be a regular expression that splits the
#'   respective value off of that compound value. See
#'   \code{\link[tidyr]{extract}} on how to set up the regular expression.
#' @param merge [\code{character(1)}]\cr In case a variable is made up of
#'   several columns, this should be the character string that would connect the
#'   two columns (e.g., an empty space \code{" "}).
#' @param distinct [\code{logical(1)}]\cr whether or not the variable is
#'   distinct from a cluster. This is the case when the variable is not
#'   systematically available for all clusters and thus needs to be registered
#'   separately from clusters.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertCharacter assertLogical
#'   testIntegerish testList
#' @importFrom dplyr case_when
#' @export

setIDVar <- function(schema = NULL, name = NULL, type = "character",
                     value = NULL, columns = NULL, rows = NULL, split = NULL,
                     merge = NULL, distinct = FALSE){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 1)
  assert(colInt, colList)
  if(colList) assertSubset(x = names(columns), choices = c("find"))
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows, len = 1)
  assert(rowInt, rowList)
  if(rowList) assertSubset(x = names(rows), choices = c("find"))
  assertCharacter(x = split, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = merge, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = distinct, any.missing = FALSE, len = 1)

  data_type <- case_when(
    type %in% c("i", "integer") ~ "integer",
    type %in% c("n", "numeric") ~ "numeric",
    type %in% c("l", "logical") ~ "logical",
    type %in% c("D", "Date") ~ "Date",
    type %in% c("_", "skip") ~ "skip",
    .default = "character"
  )

  # if type-check should be skipped, don't assert class
  if(data_type != "skip" & !is.null(value)){
    assertClass(x = value, classes = data_type)
  }

  if(is.null(schema)){
    schema <- schema_default
  }

  # update schema ----
  temp <- list(vartype = "id",
               datype = data_type,
               value = value,
               col = columns,
               row = rows,
               split = split,
               merge = merge,
               dist = distinct)
  schema@variables[[name]] <- temp

  # test for problems ----
  # reportProblems(schema = schema)

  return(schema)
}