#' Check schema description for consistency
#' @param input an input for which to check a schema description.
#' @param schema the schema description.
#' @importFrom checkmate assertList assertNames
#' @export

checkSchema <- function(input = NULL, schema = NULL){

  assertList(x = schema, len = 2)
  assertNames(x = names(schema), permutation.of = c("clusters", "variables"))
  assertNames(x = names(schema$clusters), permutation.of = c("top", "left", "width", "height", "id", "header"))

  # 1. complete cluster information ----
  clusters <- schema$clusters
  nClusters <- max(lengths(clusters))
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

  # 2. complete variables ----
  variables <- schema$variables

  outsideCluster <- NULL
  clusterID <- clusters$id
  for(i in seq_along(variables)){
    varProp <- variables[[i]]
    varName <- names(variables)[i]
    assertNames(x = names(varProp), must.include = "type")

    # make sure that all elements occur the same number of times
    if(!is.null(varProp$row)){
      if(length(varProp$row) < nClusters){
        varProp$row <- rep(x = varProp$row, length.out = nClusters)
      }
    }
    if(!is.null(varProp$col)){
      if(length(varProp$col) < nClusters){
        varProp$col <- rep(x = varProp$col, length.out = nClusters)
      }
    }

    rowIn <- TRUE
    if(!varName %in% clusterID){
      # set indices to relative values, but only if the variable is "in" a
      # cluster and if its not the clusterID, which is the case when its' row/col is larger than the smallest
      # values for a cluster
      if(!varProp$rel){
        if(!is.null(varProp$row)){
          if(!all(varProp$row < clusters$top)){
            varProp$row <- varProp$row - clusters$top + 1
            varProp$rel <- TRUE
          } else {
            rowIn <- FALSE
          }
        }
        if(!is.null(varProp$col)){
          if(!all(varProp$col < clusters$left) & rowIn){
            varProp$col <- varProp$col - clusters$left +1
            varProp$rel <- TRUE
          }
        }
      }
    } else {
      # make sure that the clusterID is not a relative value
      if(varProp$rel){
        stop(paste0("the cluster ID '", varName, "' must not have relative values"))
      }
    }

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }

  out <- list(clusters = clusters, variables = variables)
  return(out)

}