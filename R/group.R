#' Combine groups of rows or columns
#'
#' @param fn [\code{character(1)}]\cr function by which selected columns or rows
#'   shall be combined.
#' @param ... [\code{integerish(1)}]\cr columns or rows that shall be combined.
#'   If there are several items provided, they will be summarised into one
#'   group that is combined according to \code{fn}.
#' @param sep [\code{character(1)}]\cr a character string to paste the terms
#'   provided in \code{...} when no \code{fn} is given (not yet supported).
#' @return the index values where the target was found.
#' @details
#' @examples
#' @importFrom checkmate assertFunction
#' @importFrom rlang enquo enquos eval_tidy
#' @importFrom purrr map
#' @export

.group <- function(fn, sep = NULL, ...){

  assertFunction(x = fn, null.ok = TRUE)
  # assertCharacter(sep, len = 1, null.ok = TRUE)

  temp <- enquo(fn)
  grps <- unlist(enquos(...))

  if(length(grps) > 1){
    grps <- map(seq_along(grps), function(ix){
      eval_tidy(grps[[ix]])
    }) %>%
      unlist() %>%
      unique() %>%
      sort()
  }

  out <- list(group = list(by = temp, groups = grps))

  return(out)

}