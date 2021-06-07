#' Derive metadata from a schema description
#'
#' This function scrutinises the schema description in response to the data to
#' prepare metadata that are required when reshaping the input data.
#' @param data a list containing the values of \code{selectData}.
#' @param schema the schema description that is the basis to derive metadata.
#' @return A list of metadata for a single cluster, that simplifies reshaping
#'   the data.
#' @importFrom checkmate assertList assertNames
#' @importFrom stats setNames
#' @importFrom dplyr summarise across everything mutate_if select na_if
#' @importFrom tidyr unite
#' @importFrom rlang is_character

.getMetadata <- function(data = NULL, schema = NULL){

  assertList(x = data)
  assertClass(x = schema, classes = "schema")

  variables <- schema@variables
  clusters <- schema@clusters

  out <- list()
  for(j in seq_along(data)){

    theData <- data[[j]]$data
    theHeader <- data[[j]]$header
    clustRows <- data[[j]]$cluster_rows
    clustCols <- data[[j]]$cluster_cols
    tableRows <- seq_along(clustRows)
    tableRows <- tableRows[clustRows]
    dataRows <- rep(TRUE, length(tableRows))
    clustDim <- c(clusters$row[j], clusters$row[j]+clusters$height[j]-1,
                  clusters$col[j], clusters$col[j]+clusters$width[j]-1)

    varRows <- sapply(seq_along(variables), function(x){
      variables[[x]]$row
    })

    # define some variables
    idVars <- valVars <- valFctrs <- tidyVars <- tidyRel <- outVar <- spreadVars <-
      spreadTarget <- gatherVars <- splitCols <- tidyCols <- spreadCols <- gatherCols <-
      mergeOrder <- valOrder <- gatherVals <- NULL
    splitVars <- mergeVars <- list()

    # go through variables and determine whether it ... ----
    for(i in seq_along(variables)){

      varProp <- variables[[i]]
      varName <- names(variables)[i]

      # ... occurs per each cluster ----
      if(varProp$dist){
        distinct <- TRUE
      } else if(varProp$type == "id" & !is.null(varProp$value)) {
        distinct <- TRUE
      } else {
        distinct <- FALSE
      }

      # ... is an id variable ----
      if(varProp$type == "id"){
        idVars <- c(idVars, varName)

        # determine tidy id variables
        if(is.null(varProp$row) & !distinct & is.null(varProp$merge) & is.null(varProp$split)){
          tidyVars <- c(tidyVars, varName)
          tidyCols <- c(tidyCols, varProp$col[j])
        }
      }

      # ... is a values variable ----
      if(varProp$type == "observed"){
        valVars <- c(valVars, varName)
        valFctrs <- c(valFctrs, varProp$factor)

        # if the variable occurs in as many columns as there are clusters, it is
        # only in one column per cluster, and thus tidy
        if(is.null(varProp$key)){
          if(length(data) == length(varProp$col)){ #this is a useless test, see 'schema_agcensus2'
            tidyVars <- c(tidyVars, varName)
            tidyCols <- c(tidyCols, varProp$col[j])
          }
        } else if(varProp$key == "cluster") {
          if(length(unique(varProp$col)) == 1 & varProp$value == j){
            tidyVars <- c(tidyVars, varName)
            tidyCols <- c(tidyCols, unique(varProp$col))
          }

        }
      }

      # ... is outside of a cluster ----
      outsideRows <- outsideCols <- FALSE
      if(!distinct){
        # in case this variable is the group ID, it doesn't have as many values
        # as there are clusters, thus, select the position via the "$member"
        # field
        if(varName %in% clusters$group){
          pos <- clusters$member[j]
        } else {
          pos <- j
        }

        if(!is.null(varProp$row)){
          if(varProp$row[pos] < clustDim[1] | varProp$row[pos] > clustDim[2]){
            outsideRows <- TRUE
          }
        }
        if(!is.null(varProp$col)){
          if(all(varProp$col[pos] < clustDim[3] | varProp$col[pos] > clustDim[4])){
            outsideCols <- TRUE
          }
        }
      }

      if((outsideRows | outsideCols | distinct) & !varName %in% clusters$id){
        outVar <- c(outVar, varName)
      }

      # ... needs to be gathered/spread ----
      if(varProp$type == "id"){
        if(!is.null(varProp$row) & !distinct){
          # if it is cluster ID, don't gather/spread ...
          if(!varName %in% c(clusters$id, clusters$group)){
            gatherVars <- c(gatherVars, varName)
            gatherCols <- c(gatherCols, varProp$col - clusters$col[j] + 1)
          }
        }
      } else {
        # if a row has been registered, use this to derive spread/gather information
        if(!is.null(varProp$row) & !distinct){
          spreadVars <- c(spreadVars, "key")
          tempTarget <- unique(unlist(theHeader[unique(varProp$row), unique(varProp$col)])) # here it allows me to do some more error management, when these terms should not be unique
          assertCharacter(x = tempTarget, len = 1)
          spreadTarget <- c(spreadTarget, tempTarget)
          gatherCols <- c(gatherCols, varProp$col)
          spreadCols <- length(idVars) + 2
        } else if(!is.null(varProp$key)) {
          # if not that but a key is given, use the key
          spreadVars <- c(spreadVars, varProp$key)
          if(varProp$key == "cluster"){
            if(varProp$value == j){
              spreadTarget <- c(spreadTarget, as.character(varProp$value))
            }
          } else {
            spreadTarget <- c(spreadTarget, varProp$value)
          }
          if(is.null(gatherVars)){
            spreadCols <- c(spreadCols, varProp$col[j] - clusters$col[j] + 1)
          } else {
            spreadCols <- length(idVars) + 2
          }
        }
      }
      if(!is.null(spreadVars)){
        if(any(spreadVars %in% "key")){
          gatherVars <- c(gatherVars, "key")
        }
      }
      spreadVars <- unique(spreadVars)
      gatherVars <- unique(gatherVars)
      spreadCols <- unique(spreadCols)
      gatherCols <- unique(gatherCols)

      # ... occupies a row in the cluster ----
      if(!distinct){
        if(!is.null(varProp$row)){

          # only add merge row when it hasn't been added yet
          if(!any(mergeOrder %in% varProp$row[j])){
            dataRows[which(tableRows %in% varProp$row[j])] <- FALSE
            # if it is cluster ID or only in a single cell, don't merge
            if(!varName %in% clusters$id & length(varProp$col) != 1){
              mergeOrder <- c(mergeOrder, varProp$row[j])
            }
          }
        }
      }

      # ... should be merged ----
      if(!is.null(varProp$merge)){
        tempMerge <- list(mergeCols = varProp$col,
                          mergeExpr = varProp$merge)
        mergeVars <- c(mergeVars, setNames(object = list(tempMerge), nm = varName))
      }

      # ... should be split ----
      if(!is.null(varProp$split)){

        # if it is cluster ID, it is at the end of the data frame
        if(varName %in% clusters$id){
          split_col <- length(data[[j]]$cluster_cols) + 1
          split_remove <- split_col + 1
        } else {
          split_col <- varProp$col
          split_remove <- split_col + 1
        }

        tempSplit <- list(splitCol = split_col,
                          splitExpr = paste0("(", varProp$split, ")"),
                          splitRemove = split_remove)
        splitVars <- c(splitVars, setNames(object = list(tempSplit), nm = varName))
      }

      # end
    }

    # set mergeOrder so that it starts at 1 and remove merges when a measured
    # variable is the only variable in a row
    if(!is.null(mergeOrder)){
      mergeOrder <- mergeOrder - min(mergeOrder, na.rm = TRUE) + 1
    }

    # get names
    if(!is.null(gatherVars)){
      theNames <- suppressMessages(
        theHeader %>%
          t() %>%
          as_tibble(.name_repair = "unique") %>%
          mutate_if(is_character, list(~na_if(.,""))) %>%
          fill(1) %>%
          select(mergeOrder) %>%
          unite(col = "name", sep = "-_-_", na.rm = TRUE) %>%
          unlist())
    } else {
      theNames <- theHeader %>%
        unlist()
    }

    for(k in seq_along(theNames)){

      testVar <- theNames[k]

      # if the recent name is not from a tidy column, remove it ...
      if(!k %in% tidyCols){
        next
      }
      isClust <- isOut <- FALSE
      if(!is.null(clusters$id)){
        if(testVar == clusters$id){
          isClust <- TRUE
        }
      }
      if(!is.null(outVar)){
        if(any(testVar %in% outVar)){
          isOut <- TRUE
        }
      }
      if(!isClust & !isOut){
        theNames[k] <- tidyVars[which(tidyCols == k)]
      }
    }
    names(theNames) <- NULL

    temp <- list(cluster = list(cluster_rows = clustRows,
                                outside_cluster = outVar),
                 var_type = list(ids = idVars,
                                 vals = valVars,
                                 factor = valFctrs,
                                 names = theNames),
                 table = list(data_rows = dataRows,
                              tidy = tidyVars,
                              gather_into = gatherVars,
                              gather_cols = gatherCols,
                              spread_from = spreadVars,
                              spread_cols = spreadCols,
                              spread_target = spreadTarget,
                              merge = mergeVars,
                              split = splitVars)
    )
    out <- c(out, list(temp))

  }

  return(out)
}