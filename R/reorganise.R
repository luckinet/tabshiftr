#' Reorganise a table
#'
#' This function takes a disorganised messy table and rearranges columns and
#' rows into a tidy dataset that can be sorted into the areal database.
#' @param input [\code{data.frame(1)}]\cr table to reorganise.
#' @param schema [\code{symbol(1)}]\cr the schema description for reorganising
#'   \code{input}.
#' @return A (tidy) table which is the result of employing \code{schema} on
#'   \code{input}.
#' @examples
#'
#' # read in a disorganised messy dataset
#' library(readr)
#' ds <- system.file("test_datasets", package = "tabshiftr")
#' input <- read_csv(file = paste0(ds, "/table13.csv"),
#'                   col_names = FALSE, col_types = cols(.default = "c"))
#' input
#'
#' # put together schema description (see makeSchema function)
#' mySchema <- makeSchema(schema = list(
#'   clusters =
#'     list(row = c(1, 8, 8), col = c(1, 1, 4), width = 3, height = 6,
#'          id = "territories"),
#'   header = list(row = 1, rel = TRUE),
#'   variables =
#'     list(territories =
#'            list(type = "id", row = 1, col = 1, rel = TRUE),
#'          year =
#'            list(type = "id", row = c(3:6), col = 4, dist = TRUE),
#'          commodities =
#'            list(type = "id", col = 1, rel = TRUE),
#'          harvested =
#'            list(type = "measured", unit = "ha", factor = 1,
#'                 col = 2, rel = TRUE),
#'          production =
#'            list(type = "measured", unit = "t", factor = 1,
#'                 col = 3, rel = TRUE))
#' ))
#'
#' # get the tidy output
#' reorganise(input, mySchema)
#' @importFrom checkmate assertDataFrame assertList assertNames
#' @importFrom dplyr filter_all any_vars bind_rows slice group_by ungroup select
#'   mutate arrange bind_cols rename arrange_at filter mutate_if left_join
#'   mutate_all
#' @importFrom tibble rownames_to_column as_tibble add_column
#' @importFrom tidyr fill drop_na pivot_longer spread separate unite extract
#'   gather pivot_wider
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom stringr str_replace
#' @importFrom purrr reduce
#' @export

reorganise <- function(input = NULL, schema = NULL){

  # check validity of arguments
  assertDataFrame(x = input)

  input <- input %>%
    mutate_all(as.character)

  # 1. add missing information in schema ----
  schema <- updateSchema(input = input, schema = schema)
  theVariables <- schema@variables
  theClusters <- schema@clusters
  theHeader <- schema@header

  # get specs of the cluster variable
  if(!is.null(theClusters$id)){
    if(theClusters$id == "measured"){
      # in case measured variables are cluster variables, get those that contain 'key = "cluster"'
      clusterVar <- sapply(seq_along(theVariables), function(x){
        if(!is.null(theVariables[[x]]$key)){
          if(theVariables[[x]]$key == "cluster"){
            theVariables[x]
          }
        }
      })
      clusterVar <- unlist(clusterVar, recursive = FALSE)
    } else {
      clusterVar <- theVariables[theClusters$id]
    }
  } else {
    clusterVar <- NULL
  }

  # 2. use cluster information to make list of data ----
  data <- selectData(input = input,
                     clusters = theClusters,
                     header = theHeader)

  # 3. for each element in 'data' determine metadata ---
  varMeta <- getMetadata(data = data, schema = schema)

  # 4. go through all clusters and ... ----
  theValues <- list()
  for(i in seq_along(data)){

    # 5. ... rearrange the data ----
    theData <- data[[i]]$data
    theHeader <- data[[i]]$header
    theMeta <- varMeta[[i]]

    # make required columnnames
    theNames <- getNames(header = theHeader, meta = theMeta)

    # select only valid rows
    temp <- theData %>%
      filter(theMeta$table$data_rows)

    # if a column is outside of clusters, reconstruct it
    if(!is.null(theMeta$cluster$outside_cluster)){
      outVar <- theMeta$cluster$outside_cluster
      for(j in seq_along(outVar)){
        theVar <- theVariables[[which(names(theVariables) == outVar[j])]]
        if(any(outVar[j] %in% theMeta$table$tidy)){
          # tidy columns
          missingCol <- unlist(input[theMeta$cluster$cluster_rows, theVar$col[i]], use.names = FALSE)[theMeta$table$data_rows]
          temp <- temp %>%
            add_column(!!outVar[j] := missingCol)
        } else {
          # non-tidy columns
          if(!is.null(theVar$value)){
            missingVals <- theVar$value
          } else {
            if(theVar$dist){
              missingVals <- unlist(input[unique(theVar$row), unique(theVar$col)], use.names = FALSE)
            } else {
              missingVals <- unlist(input[theVar$row[i], theVar$col[i]], use.names = FALSE)
            }
          }

          temp <- temp %>%
            add_column(!!outVar[j] := missingVals)
          theMeta$table$tidy <- c(theMeta$table$tidy, outVar[j])
        }
      }
      theNames <- c(theNames, outVar)
    }

    # if a cluster id has been specified, reconstruct the column
    if(!is.null(clusterVar)){
      if(length(clusterVar) > 1){
        clusterVal <- names(clusterVar)[clusterVar[[i]]$value]
        clustName <- "cluster"

        # in case there is more than one clusterVar, it should have been
        # 'cluster_id == "values"'
      } else {
        clustName <- theClusters$id
        if(is.null(clusterVar[[1]]$value)){
          if(clusterVar[[1]]$rel){
            clusterVal <- unlist(theData[clusterVar[[1]]$row[i], clusterVar[[1]]$col[i]], use.names = FALSE)
          } else {
            clusterVal <- unlist(input[clusterVar[[1]]$row[i], clusterVar[[1]]$col[i]], use.names = FALSE)
          }
        } else {
          clusterVal <- clusterVar[[1]]$value
        }
      }
      temp <- temp %>%
        add_column(rep(unique(clusterVal), dim(temp)[1]))
      theNames <- c(theNames, clustName)
      theMeta$table$tidy <- c(theMeta$table$tidy, clustName)
    }
    if(length(clusterVar) > 1){
      valuesInCluster <- theMeta$var_type$vals[i]
    } else {
      valuesInCluster <- theMeta$var_type$vals
    }

    colnames(temp) <- theNames

    # gather all gather variables
    if(!is.null(theMeta$table$gather_into)){

      spreadCols <- valuesInCluster[1]
      temp <- temp %>%
        pivot_longer(cols = theMeta$table$gather_cols,
                     values_to = spreadCols)
      theNames <- NULL

      # ... and separate the column containing column names
      temp <- temp %>%
        separate(name, into = theMeta$table$gather_into, sep = "-_-_")
    } else {
      spreadCols <- theMeta$table$spread_cols
    }

    # spread all spread variables
    if(!is.null(theMeta$table$spread_from)){
      temp <- temp %>%
        pivot_wider(id_cols = theMeta$var_type$ids,
                    names_from = theMeta$table$spread_from,
                    values_from = spreadCols)
      theNames <- c(theMeta$var_type$ids, valuesInCluster)
    }

    # sort the data
    if(!is.null(theNames)){
      colnames(temp) <- theNames
    }

    # split id-columns that have a split expression
    if(length(theMeta$table$split) != 0){
      for(k in seq_along(theMeta$table$split)){
        theSplit <- theMeta$table$split[[k]]
        splitName <- names(theMeta$table$split)[k]
        splitCol <- which(colnames(temp) == splitName)
        if(length(splitCol) == 0){
          splitCol <- theSplit$splitCol
        }
        temp <- temp %>%
          tidyr::extract(col = splitCol,
                         into = splitName,
                         regex = theSplit$splitExpr, remove = FALSE)
      }
    }

    # if there is nothing to gather and spread, there are only tidy columns
    # available. select those, remove the first row and assign tidy names
    if(is.null(theMeta$table$gather_into) & is.null(theMeta$table$spread_from)){
      temp <- temp %>%
        select(theMeta$table$tidy)
      theNames <- NULL
    }

    # if there are columns in the table that don't have a name, remove them
    if(anyNA(names(temp))){
      temp <- temp[,which(!is.na(names(temp)))]
    }

    # make sure that all measured variables are numeric and have the correct value
    for(j in seq_along(valuesInCluster)){
      varName <- valuesInCluster[j]
      varFactor <- theMeta$var_type$factor[j]
      theVar <- temp[varName] %>% unlist(use.names = FALSE)
      theVar <- gsub(" ", "", theVar)
      if(!is.null(schema@meta$na)){
        theVar[theVar %in% schema@meta$na] <- NA
      }
      if(!is.null(schema@meta$del)){
        theVar <- gsub(schema@meta$del, "", theVar)
      }
      if(!is.null(schema@meta$dec)){
        theVar <- gsub(schema@meta$dec, ".", theVar)
      }
      theVar <- suppressWarnings(as.numeric(theVar))

      if(varFactor != 1){
        theVar <- theVar * varFactor
      }

      # replace the var in 'temp'
      temp <- temp %>%
        mutate(!!varName := as.numeric(theVar))

      if(all(is.na(theVar))){ # this could be improved further by letting the user know that XY% values other than NA/NULL/Inf were discarded and that probably the data-specs in the schema dont fit
        message(paste0("the variable '", varName, "' in cluster ", i," does not contain any numeric values.\n   -> did you set the correct the correct del(imiter)?"))
      }
    }

    temp <- temp %>%
      select(c(theMeta$var_type$ids, valuesInCluster)) %>%
      mutate_if(is.character, trimws) %>% # remove leading/trailing whitespace
      arrange_at(.vars = theMeta$var_type$ids)

    # append the data to the overall output list
    theValues <- c(theValues, list(temp))
  }

  if(length(clusterVar) > 1){
    out <- theValues %>% reduce(left_join)
  } else {
    out <- bind_rows(theValues)
  }

  return(out)
}
