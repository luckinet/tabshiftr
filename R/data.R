#' Default template of a schema description
#'
#' @format The object of class \code{schema} describes at which position in a
#'   table which information can be found. It contains the four slots
#'   \code{clusters}, \code{format}, \code{filter} and \code{variables}.
#'
#'   The default schema description contains all slots and fields that are
#'   required by default and identifying and observed variables are added to it
#'   into the \code{variables} slot.
#'
"schema_default"

#' List of table types
#'
#' @format The object of class \code{list} contains 20 different types of tables
#'   that are used throughout the unit-tests and examples/vignette.
"tabs2shift"