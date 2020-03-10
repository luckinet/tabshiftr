#' Check schema description for consistency
#' @param input an input for which to check a schema description.
#' @param schema the schema description.
#' @importFrom checkmate assertNames assertClass
#' @importFrom methods new
#' @export

updateSchema <- function(input = NULL, schema = NULL){

  assertClass(x = schema, classes = "schema")

  # 1. complete cluster information ----
  clusters <- schema@clusters
  nClusters <- max(lengths(clusters))
  if(nClusters == 0) nClusters <- 1
  tabDim <- dim(input)

  # set cluster start if it is NULL
  if(is.null(clusters$top)){
    clusters$top <- 1
  }

  if(is.null(clusters$left)){
    clusters$left <- 1
  }

  if(is.null(clusters$width)){
    clusters$width <- diff(c(clusters$left, tabDim[2]+1))
  }

  if(is.null(clusters$height)){
    if(length(clusters$top) > 1){
      clusters$height <- diff(c(clusters$top, tabDim[1]+1))
    } else {
      clusters$height <- tabDim[1]+1 - min(clusters$top)
    }
  }

  # make sure that all elements occur the same number of times
  clusters$top <- rep(x = clusters$top, length.out = nClusters)
  clusters$left <- rep(x = clusters$left, length.out = nClusters)
  clusters$width <- rep(x = clusters$width, length.out = nClusters)
  clusters$height <- rep(x = clusters$height, length.out = nClusters)

  # determine whether clusters are horizontal, vertical or messy
  if(nClusters > 1){
    clusType <- ifelse(test = length(unique(clusters$top)) == 1,
                       yes = "horizontal",
                       no = ifelse(test = length(unique(clusters$left)) == 1,
                                   yes = "vertical",
                                   no = "messy"))
  } else {
    clusType <- "none"
  }

  # 2. complete variables ----
  variables <- schema@variables

  outsideCluster <- NULL
  clusterID <- clusters$id
  for(i in seq_along(variables)){
    varProp <- variables[[i]]
    varName <- names(variables)[i]

    if(!varName %in% clusterID){

      if(!varProp$rel){
        setRel <- FALSE

        if(clusType == "horizontal"){

          if(!all(varProp$col < clusters$left)){
            leftEdge <- clusters$left
            setRel <- TRUE
          }

        } else if(clusType == "vertical"){

          if(!is.null(varProp$row)){
            if(all(varProp$row < clusters$top)){
              setRel <- FALSE
            }
          } else {
            leftEdge <- unique(clusters$left)
            setRel <- TRUE
          }


        } else if(clusType == "messy"){
          stop("messy clusters have not yet been fully implemented.")
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

    # make sure that all elements occur the same number of times
    if(!is.null(varProp$row)){
      if(length(varProp$row) == 1){
        varProp$row <- rep(x = varProp$row, length.out = nClusters)
      }
    }
    if(!is.null(varProp$col)){
      if(length(varProp$col) == 1){
        varProp$col <- rep(x = varProp$col, length.out = nClusters)
      }
    }

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }

  out <- new(Class = "schema",
             clusters = clusters,
             header = schema@header,
             meta = schema@meta,
             variables = variables)

  return(out)

}