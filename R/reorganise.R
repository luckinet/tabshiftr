#' Rectangrise a table
#'
#' This function takes the output from \code{\link{record}} and rearranges
#' columns and rows so that the resulting table has a perfectly rectangular
#' format.
#' @param input [\code{data.frame(1)}]\cr table to rectangularise.
#' @importFrom checkmate assertDataFrame
#' @importFrom dplyr filter_all any_vars bind_rows slice group_by ungroup select
#'   mutate arrange bind_cols
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom tidyr fill drop_na gather spread separate unite
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#' @export

reorganise <- function(input = NULL){

  # variables used here ----
  # varProp        = the description of a single variable
  # varName        = the name of varProp
  # varNames       = the names of all variables
  # idVars         = identifying variables of the current schema
  # idTidy         = whether or not idVars is already tidy
  # valVars        = values variables of the current schema
  # valsTidy       = whether or not valVars is already tidy
  # tidyCols       = the indices of all tidy columns
  # clusterID      = name of the identifying variable that is clustered
  # clustNames     = the names of variables within a cluster, usually that means
  #                  that it is 'varNames without clusterID'
  # outsideCluster = name of variables that are not within a cluster
  # validRows      = the rows that are part of the recent cluster

  # check validity of arguments
  assertDataFrame(x = input)

  # check whether there is already a metadata object
  if(!exists(x = "meta_object", envir = baseenv())){
    stop("please first use 'record()' to specify the table properties.")
  } else{
    current <- get(x = "meta_object", envir = baseenv())
  }

  # derive subsets for convenience
  clusters <- current$clusters
  variables <- current$variables
  origNames <- names(variables)

  # 1. clusters -----
  # set cluster start if it is NULL
  if(is.null(clusters$top)){
    clusters$top <- 1
  }
  if(is.null(clusters$left)){
    clusters$left <- 1
  }
  nClusters <- max(lengths(clusters))

  # set width and height if they are NULL
  if(is.null(clusters$width)){
    clusters$width <- diff(c(clusters$left, dim(input)[2]+1))
  }
  if(is.null(clusters$height)){
    if(length(clusters$top) > 1){
      clusters$height <- diff(c(clusters$top, dim(input)[1]+1))
    } else {
      clusters$height <- dim(input)[1]+1 - min(clusters$top)
    }
  }

  # make sure that all elements occur the same number of times
  clusters$top <- rep(x = clusters$top, length.out = nClusters)
  clusters$left <- rep(x = clusters$left, length.out = nClusters)
  clusters$width <- rep(x = clusters$width, length.out = nClusters)
  clusters$height <- rep(x = clusters$height, length.out = nClusters)

  # 2. make all position information relative ----
  idVars <- idTidy <- valVars <- valsTidy <- outsideCluster <- tidyCols <- tidyNames <- NULL
  for(i in seq_along(variables)){
    varProp <- variables[[i]]
    varName <- names(variables)[i]

    # get meta data on identifying variables
    if(varProp$type == "id"){
      if(!is.null(varProp$name)){
        varName <- varProp$name
        names(variables)[i] <- varName
      }
      idVars <- c(idVars, varName)
      if(varProp$form == "long"){
        idTidy <- c(idTidy, TRUE)
        # tidyCols <- c(tidyCols, varProp$col)
      } else {
        idTidy <- c(idTidy, FALSE)
      }
    }

    # get values variables
    if(varProp$type == "values"){
      if(!is.null(varProp$name)){
        varName <- varProp$name
        names(variables)[i] <- varName
      }
      valVars <- c(valVars, varName)
      if(is.null(varProp$id) & length(varProp$col) == 1){
        valsTidy <- c(valsTidy, TRUE)
        # tidyCols <- c(tidyCols, varProp$col)
      } else {
        valsTidy <- c(valsTidy, FALSE)
      }
    }

    # make sure that all elements occur the same number of times
    if(!is.null(varProp$row)){
      if(!all(varProp$row < clusters$top)){
        if(length(varProp$row) < nClusters){
          varProp$row <- rep(x = varProp$row, length.out = nClusters)
        }
      }
    }
    if(!is.null(varProp$col)){
      if(!all(varProp$col < clusters$left)){
        if(length(varProp$col) < nClusters){
          varProp$col <- rep(x = varProp$col, length.out = nClusters)
        }
      }
    }

    # if the variable is the 'id' of clusters, set it to be an already tidy
    # but missing column.
    if(!is.null(clusters$id)){
      if(varName == clusters$id){
        clusterID <- varName
      }
    } else {
      clusterID <- NULL
    }

    # set indices to relative values, but only if the variable is "in" a
    # cluster, which is the case when its' row/col is larger than the smallest
    # values for a cluster
    if(!varProp$rel){
      if(!is.null(varProp$row)){
        if(!all(varProp$row < clusters$top)){
          varProp$row <- varProp$row - clusters$top + 1
          varProp$rel <- TRUE
        } else {
          # separate variables that are outside of all clusters
          outsideCluster <- c(outsideCluster, varName)
        }
      }
      if(!is.null(varProp$col)){
        if(!all(varProp$col < clusters$left)){
          varProp$col <- varProp$col - clusters$left +1
          varProp$rel <- TRUE
        } else {
          outsideCluster <- c(outsideCluster, varName)
        }
      }
    }
    variables[[i]] <- varProp
  }

  # combine variable names, outNames is needed because
  varNames <- clustNames <- c(idVars, valVars)

  if(!is.null(outsideCluster)){
    # update cluster names, exclude variables that are not inside a cluster
    clustNames <- clustNames[-which(clustNames %in% outsideCluster)]

    # update likewise the identifying variables included in each cluster
    idTidy <- idTidy[-which(idVars %in% outsideCluster)]
    idVars <- idVars[-which(idVars %in% outsideCluster)]
  }

  # 3. go through all clusters and process them ----
  theValues <- theIDs <- list()
  for(i in 1:nClusters){
    # cut out table (named 'data' from here on) based on cluster information
    clusterRows <- clusters$top[i]:(clusters$top[i]+clusters$height[i] - 1)
    clusterCols <- clusters$left[i]:(clusters$left[i]+clusters$width[i] - 1)
    data <- input[clusterRows, clusterCols]
    validRows <- rep(TRUE, dim(input)[1])
    validRows[-clusterRows] <- FALSE

    # check whether any variable is the id of clusters and modify 'data'
    # according to that
    for(j in seq_along(variables)){
      varProp <- variables[[j]]
      varName <- names(variables)[j]
      if(varName %in% clusterID){
        if(!is.null(varProp$row[i]) & !is.null(varProp$col[i])){
          id <- data %>%
            slice(varProp$row[i]) %>%
            select(varProp$col[i]) %>%
            filter(!is.na(.)) %>%
            unlist(use.names = FALSE)
          data <- data %>%
            slice(-varProp$row[i])
          validRows[clusters$top[i] + varProp$row[i] - 1] <- FALSE
        } else if(!is.null(varProp$row[i])){
          id <- data %>%
            slice(varProp$row[i]) %>%
            filter(!is.na(.)) %>%
            unlist(use.names = FALSE)
          data <- data %>%
            slice(-varProp$row[i])
          validRows[clusters$top[i] + varProp$row[i] - 1] <- FALSE
        } else {
          id <- data %>%
            select(varProp$col[i]) %>%
            filter(!is.na(.)) %>%
            unlist(use.names = FALSE)
          data <- data %>%
            select(-varProp$col[i])
        }
        theIDs <- c(theIDs, list(id))

        # remove that variable from some other variables
        if(clusterID %in% clustNames){
          clustNames <- clustNames[-which(names(variables) == clusterID)]
          idVars <- idVars[-which(names(variables) == clusterID)]
          idTidy <- idTidy[-which(names(variables) == clusterID)]
        }
      }
    }

    # remove rows that have NA in all columns
    data <- data %>%
      filter_all(any_vars(!is.na(.)))

    # 4. rearrange the data ----
    spreadVars <- gatherVars <- mergeRows <- newNames <- newTidy <- NULL
    toGather <- rep(FALSE, dim(data)[2])
    # _gather information about id variables ----
    # determine which variables to gather or spread
    if(!all(idTidy)){
      # identifiers might be not tidy because they are in separate clusters, but
      # are actually tidy, or because they are really spread out over several
      # columns. Distinguish the two from each other.
      for(j in seq_along(idVars)){

        if(!idTidy[j]){
          varName <- idVars[j]
          varProp <- variables[[varName]]

          # if the variable is long within the cluster, it is made up of several
          # columns
          if(varProp$form != "long"){
            toGather[varProp$col] <- TRUE
            mergeRows <- c(mergeRows, varProp$row[i])
            gatherVars <- c(gatherVars, varName)
            spreadVars <- "key"
          } else {
            newTidy <- c(newTidy, varProp$col[i])
            if(is.null(names(newTidy))){
              tempNames <- paste0(varName, 1:length(varProp$col))
            } else {
              tempNames <- names(newTidy)
            }
            varNames <- varNames[-which(varNames %in% varName)]
          }
        }
      }
      # tidyCols <- c(newTidy, tidyCols)
      # varNames <- c(tempNames, varNames)
    }

    # If it is not the first row that has been registered for containing an
    # identifying variable, but any other row, it is likely that the first row
    # is not data, and thus it might be part of the column names (at least the
    # first row within a cluster); set it also to 'mergeRows'.
    if(!is.null(mergeRows)){
      if(!1 %in% mergeRows){
        mergeRows <- c(1, mergeRows)
      }
    }

    # _gather information about values variables ----
    # determine which variables to gather or spread
    if(!all(valsTidy)){
      for(j in seq_along(valVars)){
        if(!valsTidy[j]){
          varName <- valVars[j]
          varProp <- variables[[varName]]

          if(length(varProp$col) > 1 & is.null(clusters$id)){
            toGather[varProp$col] <- TRUE
          }
          if(!is.null(varProp$id)){
            spreadVars <- varProp$id
          } else {
            if(length(mergeRows) > length(gatherVars)){
              gatherVars <- c("key", gatherVars)
            } #else {
            #   spreadVars <- spreadVars[-which(spreadVars %in% "key")]
            #   # set the variable to NULL, if it doesn't have a content anymore
            #   if(length(spreadVars) == 0){
            #     spreadVars <- NULL
            #   }
            # }
          }
        }
      }
    }

    # fill NA to the right side of wide identifying variables (this will add the
    # value to the left of an NA instead of the NA)
    colnames(data) <- formatC(c(1:dim(data)[2]), width = nchar(dim(data)[2]), flag = "0")
    temp <- data %>%
      rownames_to_column('rn') %>%
      gather(key, val, -rn) %>%
      group_by(rn) %>%
      fill(val) %>%
      ungroup() %>%
      spread(key, val) %>%
      mutate(rn = as.numeric(rn)) %>%
      arrange(rn) %>%
      select(-rn)

# I don't know the meaning of this anymore
#     if(!any(toGather) & is.null(spreadVars)){
#       temp <- temp %>%
#         select(tidyCols) %>%
#         slice(-1)
#       newNames <- tidyNames
#     }

    if(is.null(newNames)){
      if(all(temp[1,] == origNames)){
        # if the column names are exactly the same as the first row, set the names
        # and remove the first row
        temp <- temp %>%
          slice(-1)
        newNames <- clustNames
      } else if(!is.null(mergeRows)){
        # if there are rows to merge, set column names from those.
        newNames <- temp %>%
          t() %>%
          as_tibble() %>%
          select(mergeRows) %>%
          unite(col = "name", sep = "-_-_") %>%
          unlist()
        temp <- temp %>%
          slice(-mergeRows)
      } else if(!is.null(spreadVars)) {
        # if there are variables that need to be spread, set the first row as
        # column names
        newNames <- temp %>%
          slice(1)
        temp <- temp %>%
          slice(-1)
      } else {
        newNames <- clustNames
      }
    }
    colnames(temp) <- newNames

    # gather all gather variables
    if(any(toGather)){
      temp <- temp %>%
        gather(key, values, -!!colnames(temp)[!toGather])

      # ... and separate the column containing column names
      if(!is.null(mergeRows)){
        temp <- temp %>%
          separate(key, into = c(gatherVars), sep = "-_-_")
      }
    }

    # spread long identifying variables
    if(!is.null(spreadVars)){
      temp <- temp %>%
        spread(spreadVars, value = "values")
      if(any(spreadVars %in% varNames)){
        varNames <- varNames[-which(varNames %in% spreadVars)]
      }
    }

    # if a tidy column is outside of clusters, reconstruct it
    if(!is.null(outsideCluster)){
      theColumn <- variables[[which(names(variables) == outsideCluster)]]$col
      missingCol <- input %>%
        select(!!outsideCluster := theColumn) %>%
        filter(validRows)
      temp <- temp %>%
        bind_cols(missingCol)
    }

    # if a cluster id has been specified, reconstruct the column
    if(!is.null(clusters$id)){
      temp <- temp %>%
        mutate(!!clusters$id := theIDs[[i]]) %>%
        select(names(variables))
    }
    # append cluster to the overall output list
    theValues <- c(theValues, list(temp))

  }

  # row bind the values of all clusters
  out <- bind_rows(theValues)
  colnames(out) <- varNames

  return(out)
}
