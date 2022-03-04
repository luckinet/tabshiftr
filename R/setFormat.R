#' Set the specific format of a table
#'
#' Any table makes some assumptions about the data, but they are mostly not
#' explicitly recorded in the commonly available table format. This concerns,
#' for example, the symbol(s) that signal "not available" values or the symbol
#' that is used as decimal sign.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param decimal [\code{character(1)}]\cr The symbols that should be
#'   interpreted as decimal separator.
#' @param thousand [\code{character(1)}]\cr The symbols that should be
#'   interpreted as thousand separator.
#' @param na_values [\code{character(.)}]\cr The symbols that should be
#'   interpreted as \code{NA}.
#' @param flags [\code{data.frame(2)}]\cr The typically character based flags
#'   that should be shaved off of observed variables to make them identifiable
#'   as numeric values. This must be a data.frame with two columns with names
#'   \code{flag} and \code{value}.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom dplyr bind_rows
#' @export

setFormat <- function(schema = NULL, decimal = NULL, thousand = NULL,
                      na_values = NULL, flags = NULL){

  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = decimal, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = thousand, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = na_values, any.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = flags, any.missing = FALSE, ncols = 2, null.ok = TRUE)
  if(!is.null(flags)){
    assertNames(x = names(flags), must.include = c("flag", "value"))
  }

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(decimal)){
    schema@format$dec <- decimal
  }

  if(!is.null(thousand)){
    schema@format$del <- thousand
  }

  if(!is.null(na_values)){
    schema@format$na <- na_values
  }

  if(!is.null(flags)){
    schema@format$flags <- bind_rows(schema@format$flags, flags)
  }

  return(schema)
}