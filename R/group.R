#' Group rows or columns
#'
#' @param fn [\code{character(1)}]\cr function by which selected columns or rows
#'   shall be combined.
#' @param ... [\code{integerish(1)}]\cr columns or rows that shall be combined.
#' @return the index values where the target was found.
#' @details
#' @examples
#' @importFrom checkmate assertFunction
#' @importFrom rlang enquo enquos
#' @export

.group <- function(fn, ...){

  assertFunction(x = fn)

  temp <- enquo(fn)
  grps <- unlist(enquos(...))

  out <- list(group = list(by = temp, groups = grps))

  return(out)

}