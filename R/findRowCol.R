#' Determine row or column on the fly
#'
#' Find the location of a variable not based on it's columns/rows, but based on
#' a regular expression/name
#' @param pattern [\code{character(1)}]\cr character string containing a regular
#'   expression to be matched in the \code{input} rows or columns.
#' @details This functions is basically a wild-card for when columns or rows
#'   are not known ad-hoc, but have to be assigned on the fly (for example when
#'   tables of a series are largely the same, but some variable(s) sit in
#'   different columns).
#' @return the index values where \code{pattern} was found.
#' @importFrom checkmate assertDataFrame assertCharacter
#' @importFrom purrr map_chr
#' @importFrom rlang enquo
#' @export

.find <- function(pattern){

  assertCharacter(x = pattern, min.len = 1, any.missing = FALSE)

  return(enquo(pattern))

}