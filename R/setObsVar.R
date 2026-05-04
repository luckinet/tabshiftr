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
#' @importFrom checkmate testIntegerish testList
#' @importFrom dplyr case_when
#' @export

setObsVar <- function(schema = NULL, name = NULL, type = "numeric",
                      columns = NULL, top = NULL, distinct = FALSE, factor = 1,
                      key = NULL, value = NULL){

  # assertions ----
  if(!is.null(schema) && !inherits(schema, "schema"))
    stop("setObsVar(): 'schema' must be a schema object (created by a previous setter call) or NULL.")
  if(is.null(name) || !is.character(name) || length(name) != 1 || nchar(name) == 0)
    stop("setObsVar(): 'name' must be a non-empty character string, e.g. name = \"harvested\".")
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 1)
  if(!is.null(columns) && !colInt && !colList)
    stop("setObsVar(): 'columns' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(columns), collapse = "/"), ".")
  if(colList) {
    if(!identical(names(columns), "find"))
      stop("setObsVar(): a list passed to 'columns' must be a .find() call (named 'find').")
  }
  rowInt <- testIntegerish(x = top, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = top, len = 1)
  if(!is.null(top) && !rowInt && !rowList)
    stop("setObsVar(): 'top' must be a positive integer (>= 1) or a .find() call. Got: ",
         paste(class(top), collapse = "/"), ".")
  if(rowList) {
    if(!identical(names(top), "find"))
      stop("setObsVar(): a list passed to 'top' must be a .find() call (named 'find').")
  }
  if(!is.logical(distinct) || length(distinct) != 1 || is.na(distinct))
    stop("setObsVar(): 'distinct' must be TRUE or FALSE.")
  if(!is.numeric(factor) || length(factor) != 1 || is.na(factor))
    stop("setObsVar(): 'factor' must be a single numeric value, e.g. factor = 0.40468.")
  if(is.character(key) && !identical(key, "cluster"))
    stop("setObsVar(): 'key' must be a positive integer (the column number containing variable names) ",
         "or the string \"cluster\". Got: \"", key, "\".")
  if(!is.null(key) && !is.character(key) && !testIntegerish(key, lower = 1, len = 1))
    stop("setObsVar(): 'key' must be a positive integer (>= 1) or \"cluster\". Got: ",
         paste(class(key), collapse = "/"), ".")

  # logical constraints ----
  if(!is.null(factor) && factor == 0)
    warning("setObsVar(): factor = 0 for variable '", name, "'. All values will be set to 0. ",
            "If this is intentional, ignore this warning. If not, check your unit conversion.")

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

  return(schema)
}
