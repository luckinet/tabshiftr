#' Combine groups of rows or columns
#'
#' @param fn [\code{character(1)}]\cr function by which selected columns or rows
#'   shall be combined.
#' @param ... [\code{integerish(1)}]\cr columns or rows that shall be combined.
#'   If there are several items provided, they will be summarised into one
#'   group that is combined according to \code{fn}.
#' @return the index values where the target was found.
#' @importFrom checkmate assertFunction
#' @importFrom rlang enquo enquos eval_tidy
#' @importFrom purrr map
#' @export

.group <- function(fn, ...){

  assertFunction(x = fn, null.ok = TRUE)

  temp <- enquo(fn)
  grps <- unlist(enquos(...))

  # return(grps)

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