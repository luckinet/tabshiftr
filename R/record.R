#' Record table properties
#'
#' This functions records properties of tables and stores them in a schema
#' description in preparation of reorganisation.
#' @param input [\code{data.frame(1)}]\cr table for which to record properties.
#' @param schema [\code{symbol(1)}]\cr the schema description that has been
#'   saved in the global environment.
#' @param ... [\code{various}]\cr property specific arguments; see Details.
#' @details Read the vignette for the basics of schema descriptions, a default
#'   schema description is stored in \code{\link{meta_default}}.
#' @importFrom rlang exprs
#' @importFrom checkmate assertDataFrame assertList assertNames
#' @export

record <- function(input, schema = NULL, ...){

  # check validity of arguments
  assertDataFrame(x = input)
  assertList(x = schema, len = 2)
  assertNames(x = names(schema), permutation.of = c("clusters", "variables"))

  # assign the meta data object into the base-environment
  assign(x = "meta_object", value = schema, envir = baseenv())


  return(input)

}