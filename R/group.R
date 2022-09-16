#' Determine row or column on the fly
#'
#' Find the location of a variable not based on it's columns/rows, but based on
#' a regular expression or function
#' @param fn [\code{character(1)}]\cr character string containing a regular
#'   expression or function to identify columns or rows in the input table on
#'   the fly.
#' @param ... [\code{integerish(1)}]\cr
#' @details This functions is basically a wild-card for when columns or rows are
#'   not known ad-hoc, but have to be assigned on the fly. This can be very
#'   helpful when several tables contain the same variables, but the arrangement
#'   may be slightly different.
#' @section How does this work: The first step in using any schema is validating
#'   it via the function \code{\link{validateSchema}}. This happens by default
#'   in \code{\link{reorganise}}, but can also be done manually, for example
#'   when debugging complicated schema descriptions.
#'
#'   In case that function encounters a schema that wants to find columns or
#'   rows on the fly via \code{.find}, it combines all cells of columns and all
#'   cells of rows into one character string and matches the regular expression
#'   or function on those. Columns/rows that have a match are returned as the
#'   respective column/row value.
#' @return the index values where the target was found.
#' @examples
#' @importFrom checkmate assertFunction
#' @importFrom rlang enquo enquos
#' @export

.group <- function(fn, ...){

  assertFunction(x = fn)

  temp <- enquo(fn)
  grps <- unlist(enquos(...))

  out <- list(by = temp, groups = grps)

  return(out)

}