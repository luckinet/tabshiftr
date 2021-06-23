#' Extract cluster variables
#'
#' This function extracts the cluster variable from a table by applying a schema
#' description to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map set_names
#' @export

getClusterVar <- function(input = NULL, schema = NULL){

  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))

  variables <- schema@variables
  listedObs <- map(.x = seq_along(variables), .f = function(ix){
    theVar <- variables[[ix]]
    if(theVar$type == "observed"){
      is.numeric(theVar$key)
    }
  })
  listedObs <- unique(unlist(listedObs))

  # in case a cluster ID is set, extract the value ...
  if(!is.null(clusters$id)){

    if(clusters$id == "observed"){
      out <- "observed"
    } else {
      theVar <- variables[[clusters$id]]

      if(!is.null(theVar$value)){
        temp <- tibble(X0 = theVar$value)
        out <- set_names(x = list(temp), nm = clusters$id)
      } else {

        # ... and return it as a list
        out <- map(.x = 1:nClusters, .f = function(ix){
          input[theVar$row[ix], theVar$col[ix]]
        })
        names(out) <- rep(clusters$id, nClusters)

      }
    }

  } else if(listedObs){
    out <- "observed"
  } else {
    out <- NULL
  }

  return(out)

}