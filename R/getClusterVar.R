#' Extract cluster variables
#'
#' This function extracts the cluster variable from a table by applying a schema
#' description to it.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @return list of the length of number of clusters with values cluster
#'   variable.
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
#'    getClusterVar(input = input)
#' @importFrom purrr map set_names
#' @export

getClusterVar <- function(schema = NULL, input = NULL){

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