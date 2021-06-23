#' Extract cluster group variable
#'
#' This function extracts the cluster grouping variable from a table by applying
#' a schema description to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map
#' @export

getGroupVar <- function(input = NULL, schema = NULL){

  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))

  variables <- schema@variables

  # in case a cluster ID is set, extract the value ...
  if(!is.null(clusters$group)){

    theVar <- variables[[clusters$group]]

    # ... and return it as a list
    out <- map(.x = 1:nClusters, .f = function(ix){
      input[theVar$row[ix], theVar$col[ix]]
    })
    names(out) <- rep(clusters$group, nClusters)

  } else {
    out <- NULL
  }

  return(out)

}