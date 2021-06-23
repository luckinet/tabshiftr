#' Extract clusters
#'
#' This function extracts clusters of data from a table by applying a schema
#' description to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map
#' @export

getData <- function(input = NULL, schema = NULL){

  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))

  map(.x = 1:nClusters, .f = function(ix){

    input[
      clusters$row[ix]:(clusters$row[ix]+clusters$height[ix] - 1),
      clusters$col[ix]:(clusters$col[ix]+clusters$width[ix] - 1)
      ]

  })

}