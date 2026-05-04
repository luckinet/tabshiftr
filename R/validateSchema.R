#' Check and update schema descriptions
#'
#' This function takes a raw schema description and updates values that were
#' only given as wildcard or implied values. It is automatically called by
#' \code{reorganise}, but can also be used in concert with the getters to debug
#' a schema.
#' @param input [\code{data.frame(1)}]\cr an input for which to check a schema
#'   description.
#' @param schema [\code{symbol(1)}]\cr the schema description.
#' @details The core idea of a schema description is that it can be written in a
#'   very generic way, as long as it describes sufficiently where in a table
#'   what variable can be found. A very generic way can be via using the
#'   function \code{\link{.find}} to identify the initially unknown
#'   cell-locations of a variable on-the-fly, for example when it is merely
#'   known that a variable must be in the table, but not where it is.
#'
#'   \code{validateSchema} matches a schema with an input table and inserts the
#'   accordingly evaluated positions (of clusters, filters and variables),
#'   adapts some of the meta-data and ensures formal consistency of the schema.
#' @return An updated schema description
#' @examples
#' # build a schema for an already tidy table
#' (tidyTab <- tabs2shift$tidy)
#'
#' schema <-
#'   setIDVar(name = "territories", col = 1) %>%
#'   setIDVar(name = "year", col = .find(pattern = "period")) %>%
#'   setIDVar(name = "commodities", col = 3) %>%
#'   setObsVar(name = "harvested", col = 5) %>%
#'   setObsVar(name = "production", col = 6)
#'
#' # before ...
#' schema
#'
#' # ... after
#' validateSchema(schema = schema, input = tidyTab)
#'
#' @importFrom checkmate assertNames assertClass assertNumeric
#' @importFrom rlang is_quosure
#' @importFrom dplyr mutate across ungroup n right_join
#' @importFrom tidyr replace_na everything
#' @importFrom purrr map_int map_lgl map
#' @importFrom methods new
#' @export

validateSchema <- function(schema = NULL, input = NULL){

  assertDataFrame(x = input)
  assertClass(x = schema, classes = "schema")

  filter <- schema@filter
  clusters <- schema@clusters
  groups <- schema@groups
  variables <- schema@variables
  tabDim <- dim(input)
  nRow <- tabDim[1]
  nCol <- tabDim[2]
  varNames <- names(variables)
  idVarNames <- varNames[sapply(variables, function(v) v$vartype == "id")]
  obsVarNames <- varNames[sapply(variables, function(v) v$vartype == "observed")]

  # schema coherence checks ----

  # no ID variable defined
  if(length(idVarNames) == 0)
    warning("validateSchema(): no identifying variable is defined. ",
            "Add at least one setIDVar() call to the schema before calling reorganise().")

  # no observed variable defined
  if(length(obsVarNames) == 0)
    warning("validateSchema(): no observed variable is defined. ",
            "Add at least one setObsVar() call to the schema before calling reorganise().")

  # cluster id not registered as an ID variable
  if(!is.null(clusters$id) && !clusters$id %in% c("observed", idVarNames))
    stop("validateSchema(): cluster id '", clusters$id, "' is not registered as an identifying variable. ",
         "Add setIDVar(name = \"", clusters$id, "\", ...) to the schema.")

  # group variable not registered as an ID variable
  if(!is.null(clusters$group) && !clusters$group %in% idVarNames)
    stop("validateSchema(): cluster group '", clusters$group, "' is not registered as an identifying variable. ",
         "Add setIDVar(name = \"", clusters$group, "\", ...) to the schema.")

  # cluster top out of table bounds
  if(!is.null(clusters$row) && !is.list(clusters$row)) {
    bad <- clusters$row[clusters$row > nRow]
    if(length(bad) > 0)
      stop("validateSchema(): cluster top = ", paste(bad, collapse = ", "),
           " is out of bounds. The table has only ", nRow, " rows.")
  }

  # cluster left out of table bounds
  if(!is.null(clusters$col) && !is.list(clusters$col)) {
    bad <- clusters$col[clusters$col > nCol]
    if(length(bad) > 0)
      stop("validateSchema(): cluster left = ", paste(bad, collapse = ", "),
           " is out of bounds. The table has only ", nCol, " columns.")
  }

  for(vn in varNames) {
    vp <- variables[[vn]]

    # split set but no column specified
    if(!is.null(vp$split) && is.null(vp$col) && is.null(vp$value))
      stop("validateSchema(): variable '", vn, "' has a 'split' expression but no 'columns' is set. ",
           "Provide the column(s) that contain the compound value to split.")

    # ID/obs variable column out of table bounds
    if(!is.null(vp$col) && !is.list(vp$col)) {
      bad <- vp$col[vp$col > nCol]
      if(length(bad) > 0)
        stop("validateSchema(): variable '", vn, "' references column ", paste(bad, collapse = ", "),
             ", but the table has only ", nCol, " columns.")
    }

    # ID variable rows out of table bounds
    if(vp$vartype == "id" && !is.null(vp$row) && !is.list(vp$row)) {
      bad <- vp$row[vp$row > nRow]
      if(length(bad) > 0)
        stop("validateSchema(): variable '", vn, "' references row ", paste(bad, collapse = ", "),
             " for variable names, but the table has only ", nRow, " rows.")
    }

    # obs variable top row out of table bounds
    if(vp$vartype == "observed" && !is.null(vp$row) && !is.list(vp$row)) {
      bad <- vp$row[vp$row > nRow]
      if(length(bad) > 0)
        stop("validateSchema(): variable '", vn, "' has top = ", paste(bad, collapse = ", "),
             ", but the table has only ", nRow, " rows.")
    }

    if(vp$vartype == "observed") {
      # key set without value
      if(!is.null(vp$key) && !identical(vp$key, "cluster") && is.null(vp$value))
        stop("validateSchema(): variable '", vn, "' has 'key' set to column ", vp$key,
             " but no 'value' is provided. ",
             "'value' must give the label in the key column that identifies this variable's rows ",
             "(e.g., value = \"", vn, "\").")

      # value set without key
      if(!is.null(vp$value) && is.null(vp$key))
        stop("validateSchema(): variable '", vn, "' has 'value' set but no 'key'. ",
             "For a listed (key-value) observed variable, provide both ",
             "key = <column number> and value = <label string>.")

      # key column out of table bounds
      if(!is.null(vp$key) && is.numeric(vp$key) && vp$key > nCol)
        stop("validateSchema(): variable '", vn, "' has key = ", vp$key,
             ", but the table has only ", nCol, " columns.")
    }
  }

  # inconsistent key/col across listed obs variables
  listedObs <- Filter(function(v) !is.null(v$key) && !identical(v$key, "cluster"), variables)
  if(length(listedObs) > 1) {
    keys <- sapply(listedObs, function(v) if(is.numeric(v$key)) v$key else NA)
    cols <- sapply(listedObs, function(v) if(!is.null(v$col)) paste(v$col, collapse = ",") else NA)
    if(length(unique(keys)) > 1 || length(unique(cols)) > 1) {
      detail <- paste(sapply(names(listedObs), function(n)
        paste0("  '", n, "': key = ", listedObs[[n]]$key, ", columns = ", paste(listedObs[[n]]$col, collapse = ", "))),
        collapse = "\n")
      stop("validateSchema(): all listed observed variables must share the same 'key' column and value column. ",
           "Got:\n", detail)
    }
  }

  # cluster id = "observed" but no obs var has key = "cluster"
  if(identical(clusters$id, "observed")) {
    hasClusterKey <- any(sapply(variables[obsVarNames], function(v) identical(v$key, "cluster")))
    if(!hasClusterKey)
      stop("validateSchema(): setCluster(id = \"observed\") declares that observed variables define the cluster structure, ",
           "but no observed variable has key = \"cluster\". ",
           "Each observed variable must specify which cluster it belongs to via ",
           "setObsVar(name = ..., key = \"cluster\", value = <cluster number>).")
  }

  # key = "cluster" but cluster id is not "observed"
  for(vn in obsVarNames) {
    if(identical(variables[[vn]]$key, "cluster") && !identical(clusters$id, "observed"))
      stop("validateSchema(): variable '", vn, "' uses key = \"cluster\" ",
           "but setCluster() has id = '", clusters$id, "', not id = \"observed\". ",
           "Either change to setCluster(id = \"observed\") or remove key = \"cluster\" from '", vn, "'.")
  }

  # rows set for a single-column ID variable (not wide, not distinct)
  # multi-column ID variable without rows or merge
  # merge set for a single-column variable
  # distinct = TRUE without explicit rows
  # rows length doesn't match number of cluster origins
  clusterOriginIsFind <- is.list(clusters$row) || is.list(clusters$col)
  nOrigins <- max(length(clusters$row), length(clusters$col), 1L)
  for(vn in idVarNames) {
    vp <- variables[[vn]]
    nCols <- if(is.null(vp$col) || is.list(vp$col)) 0L else length(vp$col)
    hasRows <- !is.null(vp$row) && !is.list(vp$row)
    hasMerge <- !is.null(vp$merge)

    # rows on a single-column non-distinct variable
    # skip when rows length matches nOrigins (legitimate: one label row per cluster)
    # skip when rows is a .find() list (resolved at runtime)
    rowsIsFind <- !is.null(vp$row) && is.list(vp$row)
    if(hasRows && nCols == 1L && !vp$dist && !rowsIsFind &&
       !(length(vp$row) == nOrigins && nOrigins > 1L))
      warning("validateSchema(): 'rows' is set for variable '", vn,
              "', but 'columns' has only one entry (column ", vp$col, "). ",
              "'rows' is only meaningful when the variable spans multiple columns (wide format). ",
              "If you want a distinct variable, set distinct = TRUE and provide explicit rows.")

    # multi-column ID without rows or merge
    # skip when columns contain repeats (per-cluster pattern, e.g. c(1,1,4))
    # skip when number of unique cols == nOrigins (one col per cluster, tidy)
    # skip when col or row is a .find() list (resolved at runtime)
    colIsFind <- !is.null(vp$col) && is.list(vp$col)
    colHasRepeats <- nCols > 1L && length(unique(vp$col)) < nCols
    colMatchesClusters <- nCols > 1L && length(unique(vp$col)) == nOrigins
    rowsIsFind <- !is.null(vp$row) && is.list(vp$row)
    if(nCols > 1L && !hasRows && !hasMerge && is.null(vp$value) &&
       !colIsFind && !colHasRepeats && !colMatchesClusters && !rowsIsFind)
      warning("validateSchema(): variable '", vn, "' spans multiple columns (",
              "c(", paste(vp$col, collapse = ", "), ")) but has neither 'rows' nor 'merge' set. ",
              "For a wide variable whose names are in a header row, provide rows = <row number>. ",
              "For a variable whose values should be concatenated, provide merge = <separator>.")

    # merge on a single-column variable
    if(hasMerge && nCols == 1L)
      warning("validateSchema(): 'merge' is set for variable '", vn,
              "', but 'columns' has only one entry (column ", vp$col, "). ",
              "'merge' requires at least two columns to concatenate.")

    # distinct = TRUE without explicit rows
    if(vp$dist && !hasRows && is.null(vp$value))
      stop("validateSchema(): variable '", vn, "' has distinct = TRUE but no 'rows' is set. ",
           "A distinct variable must have explicit absolute row positions (rows = c(...)).")

    # rows length doesn't match number of cluster origins
    # skip for distinct variables (multiple rows is intentional -- they span the full range)
    # skip when cluster origins are dynamic (.find()) -- nOrigins is not yet resolved
    if(hasRows && !vp$dist && !rowsIsFind && !clusterOriginIsFind &&
       length(vp$row) > 1L && length(vp$row) != nOrigins)
      warning("validateSchema(): variable '", vn, "' has rows = c(",
              paste(vp$row, collapse = ", "), ") (length ", length(vp$row),
              ") but there are ", nOrigins, " cluster origins. ",
              "The number of row values should match the number of cluster origins.")
  }

  # variable column outside cluster boundaries (only when width is explicit)
  if(!is.null(clusters$width) && !is.list(clusters$col)) {
    clusterRanges <- Map(function(l, w) seq(l, l + w - 1L), clusters$col, clusters$width)
    allClusterCols <- unique(unlist(clusterRanges))
    for(vn in varNames) {
      vp <- variables[[vn]]
      if(!is.null(vp$col) && !is.list(vp$col) && !vn %in% c(clusters$id, clusters$group)) {
        outside <- vp$col[!vp$col %in% allClusterCols]
        if(length(outside) > 0)
          warning("validateSchema(): variable '", vn, "' references column(s) ",
                  paste(outside, collapse = ", "), " which fall outside all cluster boundaries. ",
                  "Check column assignments against cluster left/width settings.")
      }
    }
  }

  # 1. evaluate filter ----
  allRows <- 1:dim(input)[1]
  if(!is.null(filter$row)){
    filter$row <- .eval_find(input = input, row = filter$row)
  }
  if(!is.null(filter$col)){
    filter$col <- .eval_find(input = input, col = filter$col)
  }

  # filter rows exclude all data rows
  if(!is.null(filter$row) && is.numeric(filter$row) && length(filter$row) > 0) {
    if(all(filter$row %in% allRows) && length(setdiff(allRows, filter$row)) == 0)
      warning("validateSchema(): after applying the row filter, no data rows remain. ",
              "Check that setFilter(rows = ...) selects data rows, not only header rows. ",
              "If you intend to exclude rows rather than keep them, set invert = TRUE.")
  }

  # 2. complete cluster information ----
  # set cluster start if it is NULL or a qousure
  if(is.null(clusters$row)){
    clusters$row <- 1
  } else if(is.list(clusters$row)){
    clusters$row <- .eval_find(input = input, row = clusters$row, clusters = clusters)

    # ignore filter rows
    if(filter$clusters & !is.null(filter$row)){
      clusters$row <- clusters$row[clusters$row %in% filter$row]
    }
    clusters$row <- .eval_sum(input = input, groups = groups, data = clusters$row)
  }

  if(is.null(clusters$col)){
    clusters$col <- 1
  } else if(is.list(clusters$col)){
    clusters$col <- .eval_find(input = input, col = clusters$col, clusters = clusters)
  }

  if(is.null(clusters$width)){
    nPos <- table(clusters$col)
    dist <- diff(c(unique(clusters$col), tabDim[2]+1))
    clusters$width <- rep(dist, times = nPos)
  }

  if(is.null(clusters$height)){
    if(length(clusters$row) > 1){
      nPos  <- table(clusters$row)
      urows <- unique(clusters$row)
      dist  <- diff(c(urows, tabDim[1]+1))
      # subtract any all-NA separator rows that fall between clusters
      # (rows below block i and above block i+1 that are entirely NA)
      for(k in seq_len(length(urows) - 1)){
        gap <- seq(urows[k], urows[k+1] - 1)
        naRows <- which(rowSums(!is.na(input[gap, , drop = FALSE])) == 0)
        dist[k] <- dist[k] - length(naRows)
      }
      clusters$height <- rep(dist, times = nPos)
    } else {
      clusters$height <- tabDim[1]+1 - min(clusters$row)
    }
  }

  nClusters <- max(lengths(clusters))
  if(nClusters == 0) nClusters <- 1

  # make sure that all elements occur the same number of times
  clusters$row <- rep(x = clusters$row, length.out = nClusters)
  clusters$col <- rep(x = clusters$col, length.out = nClusters)
  clusters$width <- rep(x = clusters$width, length.out = nClusters)
  clusters$height <- rep(x = clusters$height, length.out = nClusters)

  # if cluster-rows should not be filtered, add them again to the filter-rows
  if(!filter$clusters & !is.null(filter$row)){
    filter$row <- unique(sort(c(filter$row, clusters$row)))
  }


  # 3. adjust variables ----
  outsideCluster <- filterOut <- isAbs <- NULL
  selectRows <- selectCols <- idCols <- NULL
  clusterID <- clusters$id
  groupID <- clusters$group

  # first, evaluate whether any variable other than clusterID or groupID has a 'row' set
  headerRows <- map(.x = seq_along(variables), .f = function(ix){
    tempName <- names(variables)[ix]
    if(!tempName %in% c(groupID, clusterID)){
      temp <- variables[[ix]]
      if(temp$vartype == "observed"){
        temp$row
      } else {
        NULL
      }
    }
  })
  headerRows <- unlist(headerRows, use.names = FALSE)

  for(i in seq_along(variables)){

    varProp <- variables[[i]]
    varName <- names(variables)[i]

    # resolve quosures from grep-ing unknown col/rows ----
    if(!is.null(varProp$row)){
      if(is.list(varProp$row)){
        varProp$row <- .eval_find(input = input, row = varProp$row, clusters = clusters)

        # ignore filter rows
        if(!is.null(filter$row)){
          varProp$row <- varProp$row[varProp$row %in% filter$row]
        }

        # ignore header rows
        if(varProp$vartype == "observed"){
          varProp$row <- varProp$row[!varProp$row %in% headerRows]
        }
      }
    }

    if(!is.null(varProp$col)){
      if(is.list(varProp$col)){
        varProp$col <- .eval_find(input = input, col = varProp$col, clusters = clusters)
      }
    }

    # check whether the variable is wide ----
    if(varProp$vartype == "observed"){
      isWide <- map_lgl(.x = seq_along(idCols), function(ix){
        if(length(varProp$col) == length(idCols[[ix]])){
          all(varProp$col == idCols[[ix]])
        } else {
          FALSE
        }
      })
      if(any(isWide) & is.null(varProp$key)){
        varProp$key <- 0
        varProp$value <- "{all_rows}"
      }
    }

    # figure out which rows to filter out
    if(!varProp$dist & !varName %in% c(groupID, clusterID)){
      if(varProp$vartype == "observed"){
        if(is.null(varProp$row)){
          if(is.null(varProp$key)){
            varProp$row <- clusters$row
          } else {
            varProp$row <- 1
          }
        }
      }

      if(!is.null(varProp$row)){
        if(is.null(names(filter$row[[1]]))){
          filterOut <- sort(unique(c(filterOut, varProp$row)))
        }
      }
    }

    if(varProp$vartype == "id"){
      if(!is.null(varProp$val)){
        varProp$dist <- TRUE
      }
      idCols <- c(idCols, list(varProp$col))
    }

    # identify all selected columns ----
    selectCols <- unique(c(selectCols, varProp$col))

    # make sure that all elements occur the same number of times ----
    if(!is.null(varProp$row)){

      if(length(varProp$row) == 1){
        varProp$row <- rep(x = varProp$row, length.out = nClusters)
      }
      if(any(varName == groupID)){
        varProp$row <- varProp$row[clusters$member]
      }
    }
    if(any(varName == groupID)){
      if(!is.null(varProp$col)){
        varProp$col <- rep(x = varProp$col, length.out = length(varProp$row))
      }
    } else {
      if(!is.null(varProp$col)){
        if(length(varProp$col) == 1){
          varProp$col <- rep(x = varProp$col, length.out = nClusters)
        }
      }
    }

    # make sure that cluster or group IDs are set to NA ----
    # that their rows can be recognised as removable, in case there is nothing
    # else in that row
    if(any(varName %in% c(clusterID, groupID))){
      for(j in seq_along(varProp$col)){
        input[varProp$row[j], varProp$col[j]] <- NA
      }
    }

    # adapt rows and columns if there are groups ----
    varProp$row <- .eval_sum(input = input, groups = groups, data = varProp$row)

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }


  # 4. remove empty rows ----
  testRows <- input[,selectCols]
  emptyRows <- which(rowSums(is.na(testRows)) == ncol(testRows))


  # 5. adapt filter and cluster position to groups ----
  filterOut <- .eval_sum(input = input, groups = groups, data = filterOut)
  allRows <- .eval_sum(input = input, groups = groups, data = allRows)
  emptyRows <- .eval_sum(input = input, groups = groups, data = emptyRows)
  groupRows <- eval_tidy(groups$rows$group$groups[[1]])

  if(!is.null(filter$row)){
    filter$row <- unique(.eval_sum(input = input, groups = groups, data = filter$row))
    filter$row <- filter$row[filter$row %in% sort(unique(allRows[!allRows %in% c(filterOut, emptyRows, groupRows)]))]
  } else {
    filter$row <- sort(unique(allRows[!allRows %in% c(filterOut, emptyRows)]))
  }


  # 6. write it all ----
  out <- new(Class = "schema",
             clusters = clusters,
             format = schema@format,
             groups = schema@groups,
             filter = filter,
             variables = variables,
             validated = TRUE)

  return(out)

}
