#' Determine row or column from a data-frame
#'
#' @param pattern [\code{character(1)}]\cr character string containing a regular
#'   expression to be matched in the \code{input} rows or columns.
#' @param input [\code{data.frame}]\cr the input data-frame to match the
#'   \code{pattern} with.
#' @details In case \code{input} is not provided or the match fails, the return
#'   value is \code{NULL}, which is an adaption to these two functions' primary
#'   use-case as helpers in building a schema description. When piping through
#'   \code{\link{reorganise}}, the function will be provided with the respective
#'   input, to determine the specific index values of that data-frame.
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