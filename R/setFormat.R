#' Set the specific format of a table
#'
#' Any table makes some assumptions about the data, but they are mostly not
#' explicitly recorded in the commonly available table format. This concerns,
#' for example, the symbol(s) that signal "not available" values or the symbol
#' that is used as decimal sign.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param header [\code{logical(1)}]\cr Whether the table was read with a header
#'   row already consumed as column names (e.g. via \code{read.csv} default).
#'   If \code{TRUE}, the column names are spliced back into the table as row 1
#'   before variable extraction. Optimally, tables are read with
#'   \code{header = FALSE} so row numbers are stable, in which case this should
#'   be left as \code{FALSE} (the default).
#' @param decimal [\code{character(1)}]\cr The symbols that should be
#'   interpreted as decimal separator.
#' @param thousand [\code{character(1)}]\cr The symbols that should be
#'   interpreted as thousand separator.
#' @param na_values [\code{character(.)}]\cr The symbols that should be
#'   interpreted as \code{NA}.
#' @param zero_values [\code{character(.)}]\cr The symbols that should be
#'   interpreted as \code{0}.
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
#' @importFrom dplyr bind_rows
#' @export

setFormat <- function(schema = NULL, header = FALSE, decimal = NULL,
                      thousand = NULL, na_values = NULL, zero_values = NULL,
                      flags = NULL){

  # assertions ----
  if(!is.null(schema) && !inherits(schema, "schema"))
    stop("setFormat(): 'schema' must be a schema object (created by a previous setter call) or NULL.")
  if(!is.logical(header) || length(header) != 1 || is.na(header))
    stop("setFormat(): 'header' must be TRUE or FALSE.")
  if(!is.null(decimal) && (!is.character(decimal) || length(decimal) != 1))
    stop("setFormat(): 'decimal' must be a single character string, e.g. decimal = \",\".")
  if(!is.null(thousand) && (!is.character(thousand) || length(thousand) != 1))
    stop("setFormat(): 'thousand' must be a single character string, e.g. thousand = \".\".")
  if(!is.null(na_values) && !is.character(na_values))
    stop("setFormat(): 'na_values' must be a character vector of strings to treat as NA, e.g. na_values = c(\"n/a\", \"-\").")
  if(!is.null(zero_values) && !is.character(zero_values))
    stop("setFormat(): 'zero_values' must be a character vector of strings to treat as 0, e.g. zero_values = c(\"F\").")
  if(!is.null(flags)){
    if(!is.data.frame(flags) || ncol(flags) != 2)
      stop("setFormat(): 'flags' must be a data.frame with exactly 2 columns named 'flag' and 'value'.")
    if(!all(c("flag", "value") %in% names(flags)))
      stop("setFormat(): 'flags' must be a data.frame with columns named 'flag' and 'value'. ",
           "Got column names: ", paste(names(flags), collapse = ", "), ".")
  }

  # logical constraints ----
  if(!is.null(decimal) && !is.null(thousand) && decimal == thousand)
    stop("setFormat(): 'decimal' and 'thousand' cannot be the same character ('", decimal, "'). ",
         "A number cannot be unambiguously parsed when both separators are identical.")

  # update schema ----
  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(header)){
    schema@format$header <- header
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

  if(!is.null(zero_values)){
    schema@format$zero <- zero_values
  }

  if(!is.null(flags)){
    schema@format$flags <- bind_rows(schema@format$flags, flags)
  }

  return(schema)
}
