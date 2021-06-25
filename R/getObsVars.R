#' Extract observed variables
#'
#' This function extracts the observed variables from a table by applying a
#' schema description to it.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @return list of the length of number of clusters with values of the observed
#'   variables per cluster
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
#'    getObsVars(input = input)
#' @importFrom purrr map set_names
#' @importFrom dplyr row_number
#' @export

getObsVars <- function(schema = NULL, input = NULL){

  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))

  variables <- schema@variables
  filter <- schema@filter

  obsVars <- map(.x = seq_along(variables), .f = function(ix){
    if(variables[[ix]]$type == "observed"){
      variables[ix]
    }
  })
  obsVars <- unlist(obsVars, recursive = FALSE)

  # if there are listed observed variables, act as if they were clusters
  filterRows <- map(.x = seq_along(variables), .f = function(ix){
    theVar <- variables[[ix]]
    if(theVar$type == "observed"){
      if(is.numeric(theVar$key)){
        which(input[[theVar$key]] %in% theVar$value)
      }
    }
  })
  if(any(lengths(filterRows) != 0)){
    listedObs <- TRUE
    filterRows <- filterRows[lengths(filterRows) != 0]
    nClusters <- length(filterRows)
  } else {
    listedObs <- FALSE
  }

  if(length(obsVars) != 0){

    out <- map(.x = 1:nClusters, .f = function(ix){
      vars <- NULL
      for(i in 1:length(obsVars)){

        tempVar <- obsVars[[i]]
        if(listedObs){
          if(i != ix){
            next
          }
          clusterRows <- filterRows[[ix]]
        } else {
          clusterRows <- clusters$row[ix]:(clusters$row[ix]+clusters$height[ix] - 1)
        }

        if(!is.null(tempVar$key)){
          if(tempVar$key == "cluster"){
            if(tempVar$value != ix){
              next
            }
            if(length(unique(tempVar$col)) == 1){
              temp <- input[clusterRows, tempVar$col[ix]]
            } else {
              temp <- input[clusterRows, tempVar$col]
            }
            theFilter <- which(clusterRows %in% filter$row)
          } else if(is.numeric(tempVar$key)){
            clusterRows <- which(input[[tempVar$key]] %in% tempVar$value)
            temp <- input[clusterRows, tempVar$col]
            theFilter <- NULL
          }
        } else {

          if(!is.null(tempVar$row[ix])){
            if(nClusters != 1){
              temp <- input[clusterRows, tempVar$col[ix]]
              theFilter <- which(clusterRows %in% filter$row)
            } else {
              temp <- input[clusterRows, tempVar$col]
              theFilter <- which(clusterRows %in% filter$row)
            }
          } else {
            temp <- input[clusterRows, tempVar$col[ix]]
            theFilter <- which(clusterRows %in% filter$row)
          }

        }

        if(!is.null(theFilter)){
          temp <- temp %>%
            filter(!row_number() %in% theFilter)
        }

        vars <- c(vars, set_names(x = list(temp), nm = names(obsVars)[i]))

      }
      return(vars)

    })

  } else {
    out <- NULL
  }

  return(out)

}
