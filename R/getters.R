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


#' Extract identifying variables
#'
#' This function extracts the identifying variables from a table by applying a
#' schema description to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map set_names
#' @importFrom dplyr row_number
#' @importFrom tidyr extract unite
#' @export

getIDVars <- function(input = NULL, schema = NULL){

  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))

  variables <- schema@variables
  filter <- schema@filter

  idVars <- map(.x = seq_along(variables), .f = function(ix){
    # unselect those id variables that are also cluster id or group
    if(variables[[ix]]$type == "id" & !names(variables)[ix] %in% c(clusters$id, clusters$group)){
      variables[ix]
    }
  })
  idVars <- unlist(idVars, recursive = FALSE)

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

  if(length(idVars) != 0){

    out <- map(.x = 1:nClusters, .f = function(ix){
      vars <- NULL
      for(i in 1:length(idVars)){

        tempVar <- idVars[[i]]
        if(listedObs){
          if(!is.null(tempVar$row)){
            tempVar$row <- rep(x = tempVar$row, length.out = nClusters)
          } else {
            tempVar$col <- rep(x = tempVar$col, length.out = nClusters)
          }
          clusterRows <- filterRows[[ix]]
        } else {
          clusterRows <- clusters$row[ix]:(clusters$row[ix]+clusters$height[ix] - 1)
        }

        if(!is.null(tempVar$value)){
          temp <- tibble(X = tempVar$value)
        } else {

          if(!is.null(tempVar$row[ix])){
            if(!tempVar$dist){
              # in case a row value is set, this means we deal with a variable that is not tidy ...
              temp <- input[tempVar$row[ix], tempVar$col]
              theFilter <- NULL
            } else {
              # ... or distinct from clusters
              temp <- input[unique(tempVar$row), unique(tempVar$col)]
              theFilter <- NULL
            }
          } else {

            if(!is.null(tempVar$merge)){
              temp <- input[clusterRows, tempVar$col]
              theFilter <- filter$row
            } else {
              temp <- input[clusterRows, tempVar$col[ix]]
              theFilter <- which(clusterRows %in% filter$row)
            }

          }

          if(!is.null(tempVar$split)){
            temp <- temp %>%
              extract(col = 1, into = names(temp), regex = paste0("(", tempVar$split, ")"))
          }
          if(!is.null(tempVar$merge)){
            newName <- paste0(names(temp), collapse = tempVar$merge)
            temp <- temp %>%
              unite(col = !!newName, sep = tempVar$merge)
          }

          if(!is.null(theFilter)){
            temp <- temp %>%
              filter(!row_number() %in% theFilter)
          }
        }

        vars <- c(vars, set_names(x = list(temp), nm = names(idVars)[i]))

      }
      return(vars)

    })



  } else {
    out <- NULL
  }

  return(out)

}

#' Extract observed variables
#'
#' This function extracts the observed variables from a table by applying a
#' schema description to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map set_names
#' @importFrom dplyr row_number
#' @export

getObsVars <- function(input = NULL, schema = NULL){

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
