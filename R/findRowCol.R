#' Determine row or column on the fly
#'
#' Find the location of a variable not based on it's columns/rows, but based on
#' a regular expression/name
#' @param pattern [\code{character(1)}]\cr character string containing a regular
#'   expression to be matched in the \code{input} rows or columns.
#' @param input [\code{data.frame}]\cr optinally the input data-frame to match
#'   the \code{pattern} with.
#' @details These functions are basically a wild-card for when columns or rows
#'   are not known ad-hoc, but have to be assigned on the fly (for example when
#'   tables of a series are largely the same, but some variable(s) sit in
#'   different columns). When used in a schema description, \code{input} would
#'   be omitted, because it is assigned during the run of
#'   \code{\link{reorganise}} based on the specific table.
#' @return the index values where \code{pattern} was found.
#' @importFrom checkmate assertDataFrame assertCharacter
#' @importFrom purrr map_chr
#' @importFrom rlang enquo
#' @name find_
NULL

#' @rdname find_
#' @export

find_row <- function(pattern, input = NULL){

  assertDataFrame(x = input, null.ok = TRUE)
  assertCharacter(x = pattern, min.len = 1, any.missing = FALSE)

  if(!is.null(input)){

    dims <- dim(input)
    strings <- map_chr(.x = 1:dims[1], .f = function(ix){
      paste0(na.omit(unlist(input[ix,], use.names = FALSE)), collapse = " ")
    })

    out <- grep(pattern = pattern, x = strings)
    return(out)

  } else{
    return(enquo(pattern))
  }
}

#' @rdname find_
#' @export

find_col <- function(pattern, input = NULL){

  assertDataFrame(x = input, null.ok = TRUE)
  assertCharacter(x = pattern, min.len = 1, any.missing = FALSE)

  if(!is.null(input)){

    dims <- dim(input)
    strings <- map_chr(.x = 1:dims[2], .f = function(ix){
      paste0(na.omit(unlist(input[,ix], use.names = FALSE)), collapse = " ")
    })

    out <- grep(pattern = pattern, x = strings)
    return(out)

  } else{
    return(enquo(pattern))
  }
}

#' @rdname find_
#' @export

.find <- function(pattern){

  assertCharacter(x = pattern, min.len = 1, any.missing = FALSE)

  return(enquo(pattern))

}