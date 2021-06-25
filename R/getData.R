#' Extract clusters
#'
#' This function extracts clusters of data from a table by applying a schema
#' description to it.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @return list of the length of number of clusters with clusters cut out from
#'   the original input
#' @examples
#' input <- tabs2shift$clusters_nested
#' schema <- setCluster(id = "sublevel",
#'                      group = "territories", member = c(1, 1, 2),
#'                      left = 1, top = c(3, 8, 15)) %>%
#'   setIDVar(name = "territories", columns = 1, rows = c(2, 14)) %>%
#'   setIDVar(name = "sublevel", columns = 1, rows = c(3, 8, 15)) %>%
#'   setIDVar(name = "year", columns = 7) %>%
#'   setIDVar(name = "commodities", columns = 2) %>%
#'   setObsVar(name = "harvested", columns = 5) %>%
#'   setObsVar(name = "production", columns = 6)
#'
#' validateSchema(schema = schema, input = input) %>%
#'    getData(input = input)
#' @importFrom purrr map
#' @export

getData <- function(schema = NULL, input = NULL){

  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))

  map(.x = 1:nClusters, .f = function(ix){

    input[
      clusters$row[ix]:(clusters$row[ix]+clusters$height[ix] - 1),
      clusters$col[ix]:(clusters$col[ix]+clusters$width[ix] - 1)
      ]

  })

}