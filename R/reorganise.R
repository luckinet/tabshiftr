#' Reorganise a table
#'
#' This function takes a disorganised messy table and rearranges columns and
#' rows into a tidy dataset that can be sorted into the areal database.
#' @param input [\code{data.frame(1)}]\cr table to rectangularise.
#' @param schema [\code{symbol(1)}]\cr the schema description for reorganising
#'   \code{input}.
#' @section Setting up schema descriptions: See \code{\link{schema_default}} for a
#'   template of schema descriptions. \enumerate{ \item Clarify which are the
#'   identifying variables and which are the values variables. \item Determine
#'   whether there are clusters and find the origin (top left \item Determine
#'   whether a table can be separated into a "long" and an "other" part. The
#'   long part would consist of columns that contain identifying variables that
#'   do not need to be rearranged and the other part would contain data that
#'   need to be rearranged. \item Find the column index of all identifying
#'   variables, \itemize{ \item if identifying variables are wide, additionally
#'   find their row index. } \item Find the column index of all values
#'   variables, \itemize{ \item if a variable is spread over several columns,
#'   write down all columns for that particular variable. } \item If the names
#'   of values variables are given as an identifying variable, give that column
#'   name as \code{id} of the values variable, together with the respective term
#'   (\code{value}) of the values variables (this indicates that this
#'   \emph{key-values pair} must be spread). \itemize{ \item if the names of
#'   values variables are not given as column names, but spread across a
#'   particular row, register a variable that describes the values variables and
#'   use that variable in the \code{id} of the values variable. } \item
#'   Determine unit and transformation factor for each values variable. }
#' @importFrom checkmate assertDataFrame assertList assertNames
#' @importFrom dplyr filter_all any_vars bind_rows slice group_by ungroup select
#'   mutate arrange bind_cols rename arrange_at filter
#' @importFrom tibble rownames_to_column as_tibble add_column
#' @importFrom tidyr fill drop_na pivot_longer spread separate unite extract
#'   gather pivot_wider
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#' @export

reorganise <- function(input = NULL, schema = NULL){

  # variables used here ----
  # theVariables  = schema of the variables
  # theClusters   = schema of the cluster
  # clusterVar    = schema of the cluster id
  # clusterVal    = unique value of the id variable of the i-th cluster
  #

  # check validity of arguments
  assertDataFrame(x = input)

  # 1. add missing information in schema ----
  schema <- checkSchema(input = input, schema = schema)
  theVariables <- schema$variables
  theClusters <- schema$clusters

  # get specs of the cluster variable
  if(!is.null(theClusters$id)){
    clusterVar <- schema$variables[[theClusters$id]]
  } else {
    clusterVar <- NULL
  }

  # 2. use cluster information to make list of data ----
  data <- selectData(input = input, clusters = theClusters)

  # 3. for each element in 'data' determine metadata ---
  varMeta <- getMetadata(data = data, schema = schema)

  # 4. go through all clusters and ... ----
  theValues <- list()
  for(i in seq_along(data)){

    # 5. ... rearrange the data ----
    theData <- data[[i]]$data
    theMeta <- varMeta[[i]]

    # fill NA to the right side of wide identifying variables (this will add the
    # value to the left of an NA instead of the NA)
    colnames(theData) <- formatC(c(1:dim(theData)[2]), width = nchar(dim(theData)[2]), flag = "0")
    temp <- theData %>%
      rownames_to_column('rn') %>%
      gather(key, val, -rn) %>%
      group_by(rn) %>%
      fill(val) %>%
      ungroup() %>%
      spread(key, val) %>%
      mutate(rn = as.numeric(rn)) %>%
      arrange(rn) %>%
      select(-rn)

    # get proper names
    theNames <- getNames(temp = temp, meta = theMeta)

    # select only valid rows
    temp <- temp %>%
      filter(theMeta$table$table_rows)

    # if a tidy column is outside of clusters, reconstruct it at the correct
    # position
    if(!is.null(theMeta$cluster$outside_cluster)){
      theColumn <- theVariables[[which(names(theVariables) == theMeta$cluster$outside_cluster)]]$col[i]
      missingCol <- unlist(input[theMeta$cluster$cluster_rows, theColumn], use.names = FALSE)
      temp <- temp %>%
        add_column(missingCol)
      theNames <- c(theNames, theMeta$cluster$outside_cluster)
    }

    # if a cluster id has been specified, reconstruct the column at the correct
    # position
    if(!is.null(clusterVar)){
      clusterVal <- unlist(input[clusterVar$row[i], clusterVar$col[i]], use.names = FALSE)
      temp <- temp %>%
        add_column(rep(unique(clusterVal), dim(temp)[1]))
      theNames <- c(theNames, theClusters$id)
    }

    colnames(temp) <- theNames

    # # if there is nothing to gather and spread, there are only tidy columns
    # # available. select those, remove the first row and assign tidy names
    if(is.null(theMeta$table$gather_into) & is.null(theMeta$table$spread_from)){
      temp <- temp %>%
        select(theMeta$table$tidy)
      theNames <- NULL
    }

    # # split id-columns that have a split expression
    if(!is.null(theMeta$table$split)){
    #
    #   for(i in seq_along(splitVars)){
    #     theSplit <- splitVars[[length(splitVars)+1 - i]]
    #     theExpr <- paste0("(", theSplit$split, ")")
    #     newName <- ifelse(is.null(theSplit$name), paste0("temp", i), theSplit$name)
    #     temp <- temp %>%
    #       extract(col = theSplit$col, into = newName, regex = theExpr, remove = FALSE)
    #   }
    #   temp <- temp %>%
    #     select(-splitCols)
    }

    # # gather all gather variables
    if(!is.null(theMeta$table$gather_into)){
    #
    #   # fix 'toGather' in case splitVars exist
    #   if(!is.null(splitVars)){
    #     cols <- sapply(seq_along(splitVars), function(x){
    #       splitVars[[x]]$col
    #     })
    #     cols <- as.data.frame(table(cols))
    #     tempGather <- as.list(toGather)
    #
    #     toGather <- unlist(lapply(seq_along(tempGather), function(x){
    #       if(any(x == cols$cols)){
    #         rep(tempGather[[x]], cols$Freq)
    #       } else {
    #         tempGather[[x]]
    #       }
    #     }))
    #   }
    #
    temp <- temp %>%
      pivot_longer(cols = theMeta$table$gather_cols)

    #   # ... and separate the column containing column names
      if(!is.null(theMeta$cluster$merge_rows)){
        temp <- temp %>%
          separate(name, into = theMeta$table$gather_into, sep = "-_-_")
      }
    }

    if(!is.null(theMeta$table$spread_from)){
      temp <- temp %>%
        pivot_wider(names_from = theMeta$table$spread_from,
                    values_from = theMeta$table$spread_cols)
      theNames <- c(theMeta$var_type$ids, theMeta$var_type$vals)
    }

    # sort the data
    if(!is.null(theNames)){
      colnames(temp) <- theNames
    }
    temp <- temp %>%
      arrange_at(.vars = theMeta$var_type$ids)

    # append the data to the overall output list
    theValues <- c(theValues, list(temp))
  }


  # row bind the values of all clusters
  out <- bind_rows(theValues)

  return(out)
}
