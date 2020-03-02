#' Derive metadata from a schema description
#' @param data a list containing the values of \code{selectData}.
#' @param schema the schema description that is the basis to derive metadata.
#' @importFrom checkmate assertList assertNames
#' @importFrom stats setNames
#' @export

getMetadata <- function(data = NULL, schema = NULL){

  assertList(x = data)
  assertClass(x = schema, classes = "schema")

  variables <- schema@variables
  clusters <- schema@clusters

  out <- list()
  for(j in seq_along(data)){

    theData <- data[[j]]$data
    theHeader <- data[[j]]$header
    clustRows <- data[[j]]$cluster_rows
    tableRows <- seq_along(clustRows)
    tableRows <- tableRows[clustRows]
    dataRows <- rep(TRUE, length(tableRows))

    # define some variables
    idVars <- valVars <- valFctrs <- tidyVars <- outVar <- spreadVars <- gatherVars <- NULL
    splitCols <- tidyCols <- spreadCols <- gatherCols <- NULL
    mergeOrder <- valOrder <- tableVars <- gatherVals <- NULL
    splitVars <- list()

    # go through variables and determine whether it ... ----
    for(i in seq_along(variables)){

      varProp <- variables[[i]]
      varName <- names(variables)[i]
      assertNames(x = names(varProp), must.include = "type")

      # ... is an id variable ----
      if(varProp$type == "id"){
        assertNames(x = names(varProp), permutation.of = c("type", "name", "split", "row", "col", "rel"), .var.name = varName)
        idVars <- c(idVars, varName)
        if(!is.null(varProp$name)){
          tableVars <- c(tableVars, varProp$name)
        } else {
          tableVars <- c(tableVars, varName)
        }

        # determine tidy id variables
        if(is.null(varProp$row) | varName %in% clusters$id){
          tidyVars <- c(tidyVars, varName)
          tidyCols <- c(tidyCols, varProp$col[j])
        }
      }

      # ... is a values variable ----
      if(varProp$type == "values"){
        assertNames(x = names(varProp), permutation.of = c("type", "unit", "factor", "row", "col", "rel", "key", "value"), .var.name = varName)
        valVars <- c(valVars, varName)
        valFctrs <- c(valFctrs, varProp$factor)

        # if the variable occurs in as many columns as there are clusters, it is
        # only in one column per cluster, and thus tidy
        if(is.null(varProp$key)){
          if(length(data) == length(varProp$col)){ #this is a useless test, see 'schema_agcensus2'
            tidyVars <- c(tidyVars, varName)
            tidyCols <- c(tidyCols, varProp$col[j])
          }
        }
      }

      # ... is outside of a cluster ----
      outsideRows <- outsideCols <- FALSE
      if(!varProp$rel){
        if(!is.null(varProp$row)){
          if(all(varProp$row[j] < clusters$top[j])){
            outsideRows <- TRUE
          }
        }
        if(!is.null(varProp$col)){
          if(all(varProp$col[j] < clusters$left[j])){
            outsideCols <- TRUE
          }
        }
      }

      if((outsideRows | outsideCols) & !varName %in% clusters$id){
        outVar <- c(outVar, varName)
      }

      # ... needs to be gathered/spread ----
      if(varProp$type == "id"){
        if(!is.null(varProp$row)){
          # if it is cluster ID, don't gather/spread ...
          if(!varName %in% clusters$id){
            gatherVars <- c(gatherVars, varName)
            if(!varProp$rel){
              gatherCols <- c(gatherCols, varProp$col - clusters$left[j] + 1)
            } else {
              gatherCols <- c(gatherCols, varProp$col)
            }
          }
        }
      } else {
        # if a row has been registered, use this to derive spread/gather information
        if(!is.null(varProp$row)){
          spreadVars <- c(spreadVars, "key")
          gatherCols <- c(gatherCols, varProp$col)
          spreadCols <- length(idVars) + 2
        } else if(!is.null(varProp$key)) {
          # if not that but a key is given, use the key
          spreadVars <- c(spreadVars, varProp$key)
          if(is.null(gatherVars)){
            spreadCols <- c(spreadCols, varProp$col)
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
      if(!is.null(varProp$row)){
        # only add merge row when it hasn't been added yet
        if(!any(mergeOrder %in% varProp$row[j])){
          if(!varProp$rel){
            dataRows[which(tableRows %in% varProp$row[j])] <- FALSE
          }
          # if it is cluster ID or only in a single cell, don't merge
          if(!varName %in% clusters$id & length(varProp$col) != 1){
            mergeOrder <- c(mergeOrder, varProp$row[j])
          }
        }
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

    # get the correct order of valVars
    if(!is.null(spreadVars)){
      if(!spreadVars %in% "key"){
        theLevels <- unlist(unique(theData[,which(theHeader %in% spreadVars)]), use.names = FALSE)
        levelOrder <- sapply(seq_along(valVars), function(x){
          which(theLevels %in% variables[[valVars[x]]]$value)
        })
        valVars <- valVars[levelOrder]
      }
    }

    # set mergeOrder so that it starts at 1
    if(!is.null(mergeOrder)){
      mergeOrder <- mergeOrder - min(mergeOrder, na.rm = TRUE)+1
    }

    temp <- list(cluster = list(cluster_rows = clustRows,
                                outside_cluster = outVar,
                                cluster_id = clusters$id,
                                header = mergeOrder),
                 var_type = list(ids = idVars,
                                 orig = tableVars,
                                 vals = valVars,
                                 factor = valFctrs),
                 table = list(data_rows = dataRows,
                              tidy = tidyVars,
                              tidy_cols = tidyCols,
                              gather_into = gatherVars,
                              gather_cols = gatherCols,
                              spread_from = spreadVars,
                              spread_cols = spreadCols,
                              split = splitVars)
    )
    out <- c(out, list(temp))

  }

  return(out)
}