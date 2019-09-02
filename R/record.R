#' Record table properties
#'
#' This functions records properties of tables and stores them in a meta data
#' object in preparation of rectangularisation.
#' @param input [\code{data.frame(1)}]\cr table for which to record properties.
#' @param schema [\code{symbol(1)}]\cr the schema documentation that has been
#'   saved in the global environment.
#' @param ... [\code{various}]\cr property specific arguments; see Details.
#' @details Property-specific arguments are:\itemize{ \item \code{algorithm}:
#'   \itemize{\item \code{name = ...}} An algorithm is a list of table
#'   properties. When calling it, \code{register} sets all of the properties
#'   specified in the algorithm. \item \code{format:} \itemize{ \item \code{type
#'   = "long" | "wide"} - if each variable is in one column, the table is long;
#'   if some variables are spread over several columns, the table is wide.}
#'   \item \code{clusters:} \itemize{\item \code{row} - the row number(s) of the
#'   top-left corner of each cluster. \item \code{col} - the column number(s) of
#'   the top-left corner of each cluster. \item \code{width} - the width of
#'   clusters (in number of cells) can either have one value (when all clusters
#'   have the same width) or as many values as there are clusters. \item
#'   \code{height} - the height of clusters (in number of cells) can either have
#'   one value (when all clusters have the same height) or as many values as
#'   there are clusters.} This could for instance be \code{'row = c(2, 52), col
#'   = 3, width = 4'}, if there are 2 rows of clusters that are 4 wide, starting
#'   at cell 2,3 and 52,3 \item \code{commodities}: }
#' @importFrom rlang exprs
#' @importFrom checkmate assertDataFrame assertCharacter assertChoice assertList
#'   assertNames
#' @export

record <- function(input, schema = NULL, ...){

  # check validity of arguments
  assertDataFrame(x = input)
  # assertList(x = schema)

  meta <- eval(parse(text = schema))

  # assign the meta data object into the base-environment
  assign(x = "meta_object", value = meta, envir = baseenv())


  return(input)

}