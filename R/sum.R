#' Summarise groups of rows or columns
#'
#' @param ... [\code{integerish(1)}]\cr columns or rows that shall be combined.
#'   If there are several items provided, they will be summarised into one group
#'   that is combined according to its type and the respective function provided
#'   in \code{character} or \code{numeric}.
#' @param character [\code{function(1)}]\cr function by which character columns
#'   or rows shall be combined.
#' @param numeric [\code{function(1)}]\cr function by which numeric columns or
#'   rows shall be combined.
#' @details By default \code{character} values are summarised with the function
#'   \code{paste0(na.omit(x), collapse = "-/-")} and \code{numeric} values with
#'   the function \code{sum(x, na.rm = TRUE)}. To avoid un-intuitive behavior,
#'   it is wisest to explicitly specify how all exceptions, such as NA-values,
#'   shall be handled and thus to provide a new function.
#' @return the index values where the target was found.
#' @importFrom checkmate assertFunction
#' @importFrom rlang enquo enquos eval_tidy
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @export

.sum <- function(..., character = NULL, numeric = NULL){

  charFun <- testFunction(x = character)
  numFun <- testFunction(x = numeric)

  if(!charFun){
    character <- function(x) paste0(na.omit(x), collapse = "-/-")
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