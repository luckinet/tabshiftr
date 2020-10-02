#' Set the specific format of a table
#'
#' Any table makes some assumptions, which are mostly not explicitly recorded in
#' the commonly available table format, about the data. This concerns, for
#' example, the symbol(s) that signal "not available" values or the symbol that
#' is used as decimal sign.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param decimal [\code{character(1)}]\cr The symbols that should be
#'   interpreted as decimal sign.
#' @param delimiter [\code{character(1)}]\cr The symbols that should be
#'   interpreted as delimiter of cells.
#' @param na_values [\code{character(.)}]\cr The symbols that should be
#'   interpreted as \code{NA}.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertCharacter
#' @export

setFormat <- function(schema = NULL, decimal = NULL, delimiter = NULL, na_values = NULL){

  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = decimal, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = delimiter, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = na_values, any.missing = FALSE, null.ok = TRUE)

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(decimal)){
    schema@format$dec <- decimal
  }

  if(!is.null(delimiter)){
    schema@format$del <- delimiter
  }

  if(!is.null(na_values)){
    schema@format$na <- na_values
  }

  return(schema)
}