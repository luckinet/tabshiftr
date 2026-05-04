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
#' @importFrom checkmate testIntegerish testList
#' @importFrom dplyr case_when
#' @export

setIDVar <- function(schema = NULL, name = NULL, type = "character",
                     value = NULL, columns = NULL, rows = NULL, split = NULL,
                     merge = NULL, distinct = FALSE){

  # assertions ----
  if(!is.null(schema) && !inherits(schema, "schema"))
    stop("setIDVar(): 'schema' must be a schema object (created by a previous setter call) or NULL.")
  if(is.null(name) || !is.character(name) || length(name) != 1 || nchar(name) == 0)
    stop("setIDVar(): 'name' must be a non-empty character string, e.g. name = \"year\".")
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 1)
  if(!is.null(columns) && !colInt && !colList)
    stop("setIDVar(): 'columns' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(columns), collapse = "/"), ".")
  if(colList) {
    if(!identical(names(columns), "find"))
      stop("setIDVar(): a list passed to 'columns' must be a .find() call (named 'find').")
  }
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows, len = 1)
  if(!is.null(rows) && !rowInt && !rowList)
    stop("setIDVar(): 'rows' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(rows), collapse = "/"), ".")
  if(rowList) {
    if(!identical(names(rows), "find"))
      stop("setIDVar(): a list passed to 'rows' must be a .find() call (named 'find').")
  }
  if(!is.null(split) && (!is.character(split) || length(split) != 1))
    stop("setIDVar(): 'split' must be a single character string (a regex), e.g. split = \"(\\\\w+)_.*\".")
  if(!is.null(merge) && (!is.character(merge) || length(merge) != 1))
    stop("setIDVar(): 'merge' must be a single character string (the separator), e.g. merge = \" \".")
  if(!is.logical(distinct) || length(distinct) != 1 || is.na(distinct))
    stop("setIDVar(): 'distinct' must be TRUE or FALSE.")

  # logical constraints ----
  if(!is.null(merge) && !is.null(split))
    stop("setIDVar(): 'merge' and 'split' cannot both be set for variable '", name, "'. ",
         "Use 'merge' to join multiple columns, or 'split' to extract from a compound column -- not both.")
  if(!is.null(value) && !is.null(columns))
    stop("setIDVar(): variable '", name, "' has both 'value' and 'columns' set. ",
         "Use 'value' for implicit variables (not in the table) OR 'columns' for variables in the table, not both.")
  if(!is.null(schema) && !is.null(schema@variables[[name]]))
    message("setIDVar(): variable '", name, "' is already defined in the schema. The existing definition will be overwritten.")

  data_type <- case_when(
    type %in% c("i", "integer") ~ "integer",
    type %in% c("n", "numeric") ~ "numeric",
    type %in% c("l", "logical") ~ "logical",
    type %in% c("D", "Date") ~ "Date",
    type %in% c("_", "skip") ~ "skip",
    .default = "character"
  )

  if(data_type != "skip" && !is.null(value) && !inherits(value, data_type))
    stop("setIDVar(): 'value' must be of type '", data_type, "' (as declared in 'type'), ",
         "but got '", class(value), "'.")

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

  return(schema)
}
