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
#' @param fill [\code{character(3)}]\cr direction in which to fill missing values,
#'   possible values are "down", "up" and "right"; if several directions are
#'   required, provide them in the order required.
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

.sum <- function(..., character = NULL, numeric = NULL, fill = NULL){

  charFun <- testFunction(x = character)
  numFun <- testFunction(x = numeric)
  assertSubset(x = fill, choices = c("down", "up", "right"), empty.ok = TRUE)

  # if(!charFun){
  #   character <- function(x) paste0(na.omit(x), collapse = "-/-")
  # }
  # if(!numFun){
  #   numeric <- function(x) sum(x, na.rm = TRUE)
  # }

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

  out <- list(group = list(by = temp, groups = grps, fill = fill))

  return(out)

}