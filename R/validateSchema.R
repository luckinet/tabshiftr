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
#'   setIDVar(name = "year", col = .find("period")) %>%
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
  groups <- schema@groups
  tabDim <- dim(input)
  variables <- schema@variables

  # 1. complete cluster information ----
  clusters <- schema@clusters

  # set cluster start if it is NULL or a qousure
  if(is.null(clusters$row)){
    clusters$row <- 1
  } else if(is.list(clusters$row)){
    clusters$row <- .eval_find(input = input, row = clusters$row)
  }

  if(is.null(clusters$col)){
    clusters$col <- 1
  } else if(is.list(clusters$col)){
    clusters$col <- .eval_find(input = input, col = clusters$col)
  }

  if(is.null(clusters$width)){
    nPos <- table(clusters$col)
    dist <- diff(c(unique(clusters$col), tabDim[2]+1))
    clusters$width <- rep(dist, times = nPos)
  }

  if(is.null(clusters$height)){
    if(length(clusters$row) > 1){
      nPos <- table(clusters$row)
      dist <- diff(c(unique(clusters$row), tabDim[1]+1))
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


  # 2. evaluate filter ----
  allRows <- 1:dim(input)[1]
  if(!is.null(filter$row)){
    filter$row <- .eval_find(input = input, row = filter$row)
  }
  if(!is.null(filter$col)){
    filter$col <- .eval_find(input = input, col = filter$col)
  }


  # 3. adjust variables ----
  outsideCluster <- filterOut <- NULL
  selectRows <- selectCols <- idCols <- NULL
  clusterID <- clusters$id
  groupID <- clusters$group

  # first, evaluate whether any variable other than clusterID or groupID has a 'row' set
  headerRows <- map(.x = seq_along(variables), .f = function(ix){
    tempName <- names(variables)[ix]
    if(!tempName %in% c(groupID, clusterID)){
      temp <- variables[[ix]]
      temp$row
    }
  })
  headerRows <- unlist(headerRows, use.names = FALSE)

  for(i in seq_along(variables)){

    varProp <- variables[[i]]
    varName <- names(variables)[i]

    # resolve quosures from grep-ing unknown col/rows ----
    if(is.list(varProp$row)){
      varProp$row <- .eval_find(input = input, row = varProp$row)

      # ignore header rows
      varProp$row <- varProp$row[!varProp$row %in% headerRows]
    }

    if(is.list(varProp$col)){
      varProp$col <- .eval_find(input = input, col = varProp$col)
    }

    # check whether the variable has relative values and if so, make them absolute ----
    if(varProp$rel){
      # this might become problematic in case a schema requires several col/row to be set with a relative value
      if(!is.null(varProp$col) & !is_quosure(varProp$col) & !is.name(varProp$col)){
        varProp$col <- clusters$col + varProp$col - 1
      }
      if(!is.null(varProp$row) & !is_quosure(varProp$row) & !is.name(varProp$row)){
        varProp$row <- clusters$row + varProp$row - 1
      }
      varProp$rel <- FALSE
    }

    # check whether the variable is wide ----
    if(varProp$type == "observed"){
      isWide <- map_lgl(.x = seq_along(idCols), function(ix){
        all(varProp$col == idCols[[ix]]) & length(varProp$col) > 1
      })
      if(any(isWide) & is.null(varProp$key)){
        varProp$key <- 0
        varProp$value <- "harvested"
      }
    }

    # figure out which rows to filter out
    if(!varProp$dist & !varName %in% c(groupID, clusterID)){
      if(varProp$type == "observed"){
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

    if(varProp$type == "id"){
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
      if(!is.null(varProp$row)){
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
    varProp$row <- .eval_sum(input = input, groups = groups,
                               data = varProp$row)

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }


  # 4. remove empty rows ----
  testRows <- input[,selectCols]
  emptyRows <- which(rowSums(is.na(testRows)) == ncol(testRows))


  # 5. adapt filter and cluster position to groups ----
  clusters$row <- .eval_sum(input = input, groups = groups,
                              data = clusters$row)
  clusters$height <- .eval_sum(input = input, groups = groups,
                                 data = clusters$height)

  filterOut <- .eval_sum(input = input, groups = groups,
                           data = filterOut)
  allRows <- .eval_sum(input = input, groups = groups,
                         data = allRows)

  if(!is.null(filter$row)){
    filter$row <- filter$row[filter$row %in% sort(unique(allRows[!allRows %in% c(filterOut, emptyRows)]))]
    filter$row <- .eval_sum(input = input, groups = groups,
                              data = filter$row)
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
