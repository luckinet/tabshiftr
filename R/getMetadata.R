#' Derive metadata from a schema description
#' @param data a list containing the values of \code{selectData}.
#' @param schema the schema description that is the basis to derive metadata.
#' @importFrom checkmate assertList assertNames
#' @export

getMetadata <- function(data = NULL, schema = NULL){

  assertList(x = data)
  assertList(x = schema, len = 2)
  assertNames(x = names(schema), permutation.of = c("clusters", "variables"))
  assertNames(x = names(schema$clusters), permutation.of = c("top", "left", "width", "height", "id", "header"))

  variables <- schema$variables
  clusters <- schema$clusters

  out <- list()
  for(j in seq_along(data)){

    theData <- data[[j]]$data

    clustRows <- data[[j]]$cluster_rows
    tabRows <- rep(TRUE, dim(theData)[1])
    tabRows <- tabRows & rowSums(is.na(theData)) != ncol(theData)

    # remove header row, if it is included
    if(clusters$header){
      tabRows[1] <- FALSE
      tabNames <- theData %>%
        slice(1) %>%
        unlist()
      names(tabNames) <- NULL
    } else {
      tabNames <- NULL
    }

    # define some variables
    splitVars <- idVars <- valVars <- tidyVars <- outVar <- spreadVars <- gatherVars <- NULL
    splitCols <-                      tidyCols <-           spreadCols <- gatherCols <- NULL
    mergeRows <- NULL

    # go through variables and determine whether it ... ----
    for(i in seq_along(variables)){

      varProp <- variables[[i]]
      varName <- names(variables)[i]
      assertNames(x = names(varProp), must.include = "type")

      # replace the variable name, if it's given
      if(!is.null(varProp$name) & !is.null(tabNames)){
        tabNames[i] <- varProp$name
      }

      # ... should be split ----
      if(!is.null(varProp$split)){
        splitVars <- c(splitVars, variables[i])
        # splitCols <- c(splitCols, varProp$col)
      }
      # splitCols <- unique(splitCols)

      # ... is an id variable ----
      if(varProp$type == "id"){
        assertNames(x = names(varProp), permutation.of = c("type", "name", "split", "row", "col", "rel"), .var.name = varName)
        idVars <- c(idVars, varName)

        # determine tidy id variables
        if(is.null(varProp$row) | varName %in% clusters$id){
          tidyVars <- c(tidyVars, varName)
        }
      }

      # ... is a values variable ----
      if(varProp$type == "values"){
        assertNames(x = names(varProp), permutation.of = c("type", "unit", "factor", "row", "col", "rel", "key", "value"), .var.name = varName)
        valVars <- c(valVars, varName)

        # if the variable occurs in as many columns as there are clusters, it is
        # only in one column per cluster, and thus tidy
        if(is.null(varProp$key)){
          if(length(data) == length(varProp$col)){
            tidyVars <- c(tidyVars, varName)
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
            gatherCols <- c(gatherCols, varProp$col)
          }
        }
      } else {
        # if a row has been registered, use this to derive spread/gather information
        if(!is.null(varProp$row)){
          spreadVars <- c(spreadVars, "key")
          gatherCols <- c(gatherCols, varProp$col)
          # spreadCols <- length(idVars) + 2
        } else if(!is.null(varProp$key)) {
          # if not that but a key is given, use the key
          spreadVars <- c(spreadVars, varProp$key)
          # if(!is.null(gatherVars)){
            # spreadCols <- c(spreadCols, varProp$col)
          # }
        }
        spreadCols <- length(idVars) + 2
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
        if(!any(mergeRows %in% varProp$row[j])){
          if(varProp$rel){
            tabRows[varProp$row[j]] <- FALSE
          } else {
            tabRows[which(clustRows) %in% varProp$row[j]] <- FALSE
          }
          clustRows[varProp$row[j]] <- FALSE
          # if it is cluster ID, don't merge
          if(!varName %in% clusters$id){
            mergeRows <- c(mergeRows, varProp$row[j])
          }
        }
      }

    }

    temp <- list(cluster = list(cluster_rows = clustRows,
                                outside_cluster = outVar,
                                cluster_id = clusters$id,
                                merge_rows = mergeRows),
                 var_type = list(ids = idVars,
                                 vals = valVars),
                 table = list(header = tabNames,
                              table_rows = tabRows,
                              tidy = tidyVars,
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