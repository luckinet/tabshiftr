#' Check schema description for consistency
#'
#' This function takes an input table and a respective input schema and ensures
#' that the cluster specification is complete and that column and row values are
#' consistent.
#' @param input an input for which to check a schema description.
#' @param schema the schema description.
#' @return An updated schema description that fulfills formal requirements to be
#'   processed by \code{\link{reorganise}}.
#' @importFrom checkmate assertNames assertClass
#' @importFrom rlang is_quosure
#' @importFrom purrr map_int
#' @importFrom methods new

.updateSchema <- function(input = NULL, schema = NULL){

  assertDataFrame(x = input)
  assertClass(x = schema, classes = "schema")

  # 1. complete cluster information ----
  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))
  if(nClusters == 0) nClusters <- 1
  tabDim <- dim(input)

  # set cluster start if it is NULL or a qousure
  if(is.null(clusters$row)){
    clusters$row <- 1
  } else if(is_quosure(clusters$row)){
    term <- eval_tidy(clusters$row)
    rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
      grepl(x = paste(input[ix,], collapse = " "), pattern = term)
    })
    clusters$row <- which(rows == 1)
  }

  if(is.null(clusters$col)){
    clusters$col <- 1
  } else if(is_quosure(clusters$col)){
    term <- eval_tidy(clusters$col)
    cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
      grepl(x = paste(input[[ix]], collapse = " "), pattern = term)
    })
    clusters$col <- which(cols == 1)
  }

  if(is.null(clusters$width)){
    nPos <- table(clusters$col)
    dist <- diff(c(unique(clusters$col), tabDim[2]+1))
    clusters$width <- rep(dist, times = nPos)
  }

  if(is.null(clusters$height)){
    if(length(clusters$row) > 1){
      nPos <- table(clusters$row)
      dist <- diff(c(unique(clusters$row), tabDim[1]+1))
      clusters$height <- rep(dist, times = nPos)
    } else {
      clusters$height <- tabDim[1]+1 - min(clusters$row)
    }
  }

  # make sure that all elements occur the same number of times
  clusters$row <- rep(x = clusters$row, length.out = nClusters)
  clusters$col <- rep(x = clusters$col, length.out = nClusters)
  clusters$width <- rep(x = clusters$width, length.out = nClusters)
  clusters$height <- rep(x = clusters$height, length.out = nClusters)

  # determine whether clusters are horizontal, vertical or messy
  if(nClusters > 1){
    clusType <- ifelse(test = length(unique(clusters$row)) == 1,
                       yes = "horizontal",
                       no = ifelse(test = length(unique(clusters$col)) == 1,
                                   yes = "vertical",
                                   no = "messy"))
  } else {
    clusType <- "none"
  }

  # 2. complete header ----
  header <- schema@header
  if(is_quosure(header$row)){
    term <- eval_tidy(header$row)
    rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
      grepl(x = paste(input[ix,], collapse = " "), pattern = term)
    })
    header$row <- which(rows == 1)
  }

  # 3. complete variables ----
  variables <- schema@variables

  outsideCluster <- NULL
  clusterID <- clusters$id
  groupID <- clusters$group
  for(i in seq_along(variables)){
    varProp <- variables[[i]]
    varName <- names(variables)[i]

    ##### replace from here .... ####
    if(!varName %in% c(clusterID, groupID)){

      if(!varProp$rel){
        setRel <- FALSE

        if(clusType == "horizontal"){

          if(!all(varProp$col < clusters$col)){
            leftEdge <- clusters$col
            setRel <- TRUE
          } else {
            varProp$dist <- TRUE
          }

        } else if(clusType == "vertical"){

          if(!is.null(varProp$row)){
            if(all(varProp$row < clusters$row)){
              setRel <- FALSE
            }
          } else {
            leftEdge <- unique(clusters$col)
            setRel <- TRUE
          }


        } else if(clusType == "messy"){
          # stop("messy clusters have not yet been fully implemented.")
        }

        if(varProp$type == "id" & !is.null(varProp$val)){
          setRel <- FALSE
          varProp$dist <- TRUE
        }

        if(setRel){
          varProp$col <- varProp$col - leftEdge + 1
          varProp$rel <- TRUE
        }
      }
    }
    # else {
    #   # make sure that the clusterID is not a relative value
    #   if(varProp$rel){
    #     stop(paste0("the cluster ID '", varName, "' must not have relative values."))
    #   }
    # }
    ##### ... to here ... ####


    # ... with this
    # # check whether the variable has relative values and if so, make them absolute
    # if(varProp$rel){
    #   # this might become problematic in case a schema requires several col/row to be set with a relative value
    #   if(!is.null(varProp$col)){
    #     varProp$col <- clusters$col + varProp$col - 1
    #   }
    #   if(!is.null(varProp$row)){
    #     varProp$row <- clusters$row + varProp$row - 1
    #   }
    #   varProp$rel <- FALSE
    # }
    #
    # # check whether the variable is actually distinct (i.e., outside of clusters)
    # if(!is_quosure(varProp$col) & !varName %in% clusterID){
    #   if(all(varProp$col < clusters$col)){
    #     varProp$dist <- TRUE
    #   }
    # }
    # if(varProp$type == "id" & !is.null(varProp$val)){
    #   varProp$dist <- TRUE
    # }

    # resolve quosures from grep-ing unkown col/rows
    if(is_quosure(varProp$row)){
      term <- eval_tidy(varProp$row)
      rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
        grepl(x = paste(input[ix,], collapse = " "), pattern = term)
      })
      varProp$row <- which(rows == 1)
    }
    if(is_quosure(varProp$col)){
      term <- eval_tidy(varProp$col)
      cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
        grepl(x = paste(input[[ix]], collapse = " "), pattern = term)
      })
      varProp$col <- which(cols == 1)
    }

    # make sure that all elements occur the same number of times
    if(!is.null(varProp$row)){

      if(!any(header$row %in% varProp$row)){
        if(!varName %in% clusters$id){
          if(length(varProp$col) != 1){
            header$row <- c(header$row, varProp$row)
          }
        }
      }

      if(length(varProp$row) == 1){
        varProp$row <- rep(x = varProp$row, length.out = nClusters)
      }
      if(any(varName == groupID)){
        varProp$row <- varProp$row[clusters$member]
      }
    }
    if(any(varName == groupID)){
      if(!is.null(varProp$row)){
        varProp$col <- rep(x = varProp$col, length.out = length(varProp$row))
      }
    } else {
      if(!is.null(varProp$col)){
        if(length(varProp$col) == 1){
          varProp$col <- rep(x = varProp$col, length.out = nClusters)
        }
      }
    }

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }

  out <- new(Class = "schema",
             clusters = clusters,
             header = header,
             format = schema@format,
             filter = schema@filter,
             variables = variables)

  return(out)

}
