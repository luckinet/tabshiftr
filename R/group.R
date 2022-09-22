#' Combine groups of rows or columns
#'
#' @param ... [\code{integerish(1)}]\cr columns or rows that shall be combined.
#'   If there are several items provided, they will be summarised into one group
#'   that is combined according to \code{fn}.
#' @param character [\code{function(1)}]\cr function by which character columns
#'   or rows shall be combined.
#' @param numeric [\code{function(1)}]\cr function by which numeric columns or
#'   rows shall be combined.
#' @return the index values where the target was found.
#' @importFrom checkmate assertFunction
#' @importFrom rlang enquo enquos eval_tidy
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @export

.group <- function(..., character = NULL, numeric = NULL){

  charFun <- testFunction(x = character)
  numFun <- testFunction(x = numeric)

  if(!charFun){
    character <- function(x) paste0(na.omit(x), collapse = "-/- ")
  }
  if(!numFun){
    numeric <- function(x) sum(x, na.rm = TRUE)
  }

  temp <- list(char = enquo(character), num = enquo(numeric))
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