#' Reorganise a table
#'
#' This function takes a disorganised messy table and rearranges columns and
#' rows into a tidy table.
#' @param input [\code{data.frame(1)}]\cr table to reorganise.
#' @param schema [\code{symbol(1)}]\cr the schema description for reorganising
#'   \code{input}.
#' @return A (tidy) table which is the result of employing \code{schema} on
#'   \code{input}.
#' @examples
#'
#' # read in a disorganised messy dataset (without column names)
#' library(readr)
#' library(magrittr)
#' ds <- system.file("test_datasets", package = "tabshiftr")
#' input <- read_csv(file = paste0(ds, "/table_mismatch_3.csv"),
#'                   col_names = FALSE, col_types = cols(.default = "c"))
#' input
#'
#' # put together schema description
#' schema <- setCluster(id = "territories", top = c(1, 8, 8), left = c(1, 1, 4),
#'                      width = 3, height = 6) %>%
#'   setHeader(rows = 1, relative = TRUE) %>%
#'   setIDVar(name = "territories", columns = 1, row = 2, relative = TRUE) %>%
#'   setIDVar(name = "year", columns = 4, row = c(3:6), distinct = TRUE) %>%
#'   setIDVar(name = "commodities", columns = 1, relative = TRUE) %>%
#'   setObsVar(name = "harvested", unit = "ha", columns = 2, relative = TRUE) %>%
#'   setObsVar(name = "production", unit = "t", columns = 3, relative = TRUE)
#'
#' # get the tidy output
#' reorganise(input, schema)
#' @importFrom checkmate assertDataFrame assertList assertNames
#' @importFrom dplyr filter_all any_vars bind_rows slice group_by ungroup select
#'   mutate arrange bind_cols rename arrange_at filter mutate_if left_join
#'   mutate_all
#' @importFrom tibble rownames_to_column as_tibble add_column
#' @importFrom tidyr fill drop_na pivot_longer spread separate unite extract
#'   gather pivot_wider
#' @importFrom tidyselect everything all_of
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom stringr str_replace
#' @importFrom purrr reduce
#' @export

reorganise <- function(input = NULL, schema = NULL){

  # source('/media/se87kuhe/external1/projekte/r-dev/tabshiftr/R/updateSchema.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/tabshiftr/R/selectData.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/tabshiftr/R/getMetadata.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/tabshiftr/R/getNames.R')

  # check validity of arguments
  assertDataFrame(x = input)

  input <- input %>%
    mutate_all(as.character)

  # 1. add missing information in schema ----
  schema <- .updateSchema(input = input, schema = schema)
  theVariables <- schema@variables
  theClusters <- schema@clusters
  theFormat <- schema@format

  # 2. use cluster information to make list of data ----
  data <- .selectData(input = input, schema = schema)

  # 3. for each element in 'data' determine metadata ---
  varMeta <- .getMetadata(data = data, schema = schema)

  # 4. go through all clusters and ... ----
  theValues <- list()
  for(i in seq_along(data)){

    # 5. ... rearrange the data ----
    theData <- data[[i]]
    theTable <- input[theData$data_rows, theData$data_cols]
    theHeader <- theData$header
    theMeta <- varMeta[[i]]

    # make required columnnames
    theNames <- .getNames(header = theHeader, meta = theMeta)
    theNames <- theNames[theData$data_cols]

    colnames(theTable) <- theNames

    # if a column is outside of clusters, reconstruct it
    if(!is.null(theMeta$cluster$outside_cluster)){
      outVar <- theMeta$cluster$outside_cluster
      for(j in seq_along(outVar)){
        theVar <- theVariables[[which(names(theVariables) == outVar[j])]]
        if(any(outVar[j] %in% theMeta$table$tidy)){
          # tidy columns
          missingCol <- unlist(input[theMeta$cluster$cluster_rows, theVar$col[i]], use.names = FALSE)[theMeta$table$data_rows]
          theTable <- theTable %>%
            add_column(!!outVar[j] := missingCol)
        } else {
          # non-tidy columns
          if(!is.null(theVar$value)){
            missingVals <- theVar$value
          } else {
            if(theVar$dist){
              missingVals <- unlist(data[[i]]$outside, use.names = FALSE)
            } else {
              missingVals <- unlist(input[theVar$row[i], theVar$col[i]], use.names = FALSE)
            }
          }

          theTable <- theTable %>%
            add_column(!!outVar[j] := missingVals)
          theMeta$table$tidy <- c(theMeta$table$tidy, outVar[j])
        }
      }
      theNames <- c(theNames, outVar)
    }

    # if a cluster id has been specified, reconstruct the column
    clusterVar <- theData$cluster_var
    if(!is.null(clusterVar)){
      if(theClusters$id == "observed"){
        clusterVal <- data[[i]]$cluster_val
        clustName <- "cluster"
      #
      #   # in case there is more than one clusterVar, it should have been
      #   # 'cluster_id == "values"'
      } else {
        clustName <- theClusters$id
        if(is.null(clusterVar$value)){
          clusterVal <- data[[i]]$cluster_val
        } else {
          clusterVal <- clusterVar$value
        }
      }
      theTable <- theTable %>%
        add_column(!!clustName := rep(unique(clusterVal), dim(theTable)[1]))
      theNames <- c(theNames, clustName)
      theMeta$table$tidy <- c(theMeta$table$tidy, clustName)
    }

    if(any(theClusters$id == "observed")){
      valuesInCluster <- theMeta$var_type$vals[i]
    } else {
      valuesInCluster <- theMeta$var_type$vals
    }

    # merge id-columns that have a merge expression
    if(length(theMeta$table$merge) != 0){
      for(k in seq_along(theMeta$table$merge)){
        theMerge <- theMeta$table$merge[[k]]
        mergeName <- names(theMeta$table$merge)[k]
        mergeCols <- names(theTable)[theMerge$mergeCols]
        mergeSep <- theMerge$mergeExpr

        theTable <- theTable %>%
          unite(!!mergeName, all_of(mergeCols), sep = mergeSep)

        theMeta$table$tidy <- c(theMeta$table$tidy, mergeName)
      }
      theNames <- NULL
    }

    # complete id-columns so that there are no missing rows in them
    toComplete <- theMeta$var_type$ids
    toComplete <- toComplete[!toComplete %in% names(theMeta$table$split)]
    toComplete <- toComplete[!toComplete %in% theMeta$table$gather_into]
    theTable <- theTable %>%
      fill(all_of(toComplete))

    # gather all gather variables
    if(!is.null(theMeta$table$gather_into)){

      spreadCols <- valuesInCluster[1]
      theTable <- theTable %>%
        pivot_longer(cols = theMeta$table$gather_cols,
                     values_to = spreadCols, names_repair = "minimal")
      theNames <- NULL

      # ... and separate the column containing column names
      theTable <- theTable %>%
        separate(name, into = theMeta$table$gather_into, sep = "-_-_")
    } else {
      spreadCols <- theMeta$table$spread_cols
    }

    # spread all spread variables
    if(!is.null(theMeta$table$spread_from)){
      theTable <- theTable %>%
        pivot_wider(id_cols = theMeta$var_type$ids,
                    names_from = theMeta$table$spread_from,
                    values_from = all_of(spreadCols)) %>%
        select(theMeta$var_type$ids, theMeta$table$spread_target)
      theNames <- c(theMeta$var_type$ids, valuesInCluster)
    }

    # sort the data
    if(!is.null(theNames)){
      colnames(theTable) <- theNames
    }

    # split id-columns that have a split expression
    if(length(theMeta$table$split) != 0){
      for(k in seq_along(theMeta$table$split)){
        if(k == length(theMeta$table$split)){
          doRemove <- TRUE
        } else {
          doRemove <- FALSE
        }
        theSplit <- theMeta$table$split[[k]]
        splitName <- names(theMeta$table$split)[k]
        splitCol <- which(colnames(theTable) == splitName)
        if(length(splitCol) == 0){
          splitCol <- theSplit$splitCol
        }
        theTable <- theTable %>%
          tidyr::extract(col = splitCol,
                         into = splitName,
                         regex = theSplit$splitExpr,
                         remove = doRemove)
        theMeta$table$tidy <- c(theMeta$table$tidy, splitName)
      }
      theNames <- NULL
    }

    # if there is nothing to gather and spread, there are only tidy columns
    # available. select those, remove the first row and assign tidy names
    if(is.null(theMeta$table$gather_into) & is.null(theMeta$table$spread_from)){
      theTable <- theTable %>%
        select(theMeta$table$tidy)
      theNames <- NULL
    }

    # if there are columns in the table that don't have a name, remove them
    if(anyNA(names(theTable))){
      theTable <- theTable[,which(!is.na(names(theTable)))]
    }
    if(any(names(theTable) == "")){
      theTable <- theTable[,-which(names(theTable) == "")]
    }

    # make sure that all observed variables are numeric and have the correct value
    for(j in seq_along(valuesInCluster)){
      varName <- valuesInCluster[j]
      varFactor <- theMeta$var_type$factor[j]
      theVar <- theTable[varName] %>% unlist(use.names = FALSE)
      theVar <- gsub(" ", "", theVar)
      if(!is.null(theFormat$na)){
        theVar[theVar %in% theFormat$na] <- NA
      }
      if(!is.null(theFormat$del)){
        if(theFormat$del == "."){
          theFormat$del <- "[.]"
        }
        theVar <- gsub(theFormat$del, "", theVar)
      }
      if(!is.null(theFormat$dec)){
        if(theFormat$dec == "."){
          theFormat$dec <- "[.]"
        }
        theVar <- gsub(theFormat$dec, ".", theVar)
      }
      theVar <- suppressWarnings(as.numeric(theVar))

      if(varFactor != 1){
        theVar <- theVar * varFactor
      }

      # replace the var in 'theTable'
      theTable <- theTable %>%
        mutate(!!varName := as.numeric(theVar))

      if(all(is.na(theVar))){ # this could be improved further by letting the user know that XY% values other than NA/NULL/Inf were discarded and that probably the data-specs in the schema dont fit
        message(paste0("the variable '", varName, "' in cluster ", i," does not contain any numeric values.\n   -> did you set the correct the correct del(imiter)?"))
      }
    }

    theTable <- theTable %>%
      select(c(theMeta$var_type$ids, all_of(valuesInCluster))) %>%
      mutate_if(is.character, trimws) %>% # remove leading/trailing whitespace
      arrange_at(.vars = theMeta$var_type$ids)

    # append the data to the overall output list
    theValues <- c(theValues, list(theTable))
  }

  if(any(theClusters$id == "observed")){
    out <- theValues %>% reduce(left_join)
  } else {
    out <- bind_rows(theValues)
  }

  return(out)
}
