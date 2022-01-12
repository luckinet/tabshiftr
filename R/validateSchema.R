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
#' @importFrom checkmate assertNames assertClass
#' @importFrom rlang is_quosure
#' @importFrom dplyr mutate across
#' @importFrom tidyr replace_na everything
#' @importFrom purrr map_int map_lgl
#' @importFrom methods new
#' @export

validateSchema <- function(schema = NULL, input = NULL){

  assertDataFrame(x = input)
  assertClass(x = schema, classes = "schema")

  # 1. complete cluster information ----
  clusters <- schema@clusters
  filter <- schema@filter
  tabDim <- dim(input)
  variables <- schema@variables

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


  # 2. complete filter ----
  # evaluate quosure
  if(is.list(filter$row)){
    filter$row <- .eval_find(input = input, row = filter$row)
  }

  if(!filter$invert){
    if(!is.null(filter$row)){
      filter$row <- (1:tabDim[1])[-filter$row]
    }
  }

  topAfterFilter <- min(which(!1:dim(input)[1] %in% filter$row))

  # 3. complete variables ----
  outsideCluster <- NULL
  selectRows <- selectCols <- NULL
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

    # resolve quosures from grep-ing unkown col/rows ----
    if(is.list(varProp$row)){
      varProp$row <- .eval_find(input = input, row = varProp$row)

      # ignore header rows
      varProp$row <- varProp$row[!varProp$row %in% headerRows]
    }

    if(is.list(varProp$col)){
      varProp$col <- .eval_find(input = input, col = varProp$col, row = varProp$row)
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

        if(any(varProp$row < topAfterFilter)){
          varProp$row[which(varProp$row < topAfterFilter)] <- topAfterFilter
        }
      }

      # build selectCols and assign it to filter$row
      filter$row <- sort(unique(c(filter$row, varProp$row)))
    }

    if(varProp$type == "id" & !is.null(varProp$val)){
      varProp$dist <- TRUE
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

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }

  # 4. remove empty rows ----
  testRows <- input[,selectCols]
  emptyRows <- which(rowSums(is.na(testRows)) == ncol(testRows))
  filter$row <- sort(unique(c(filter$row, emptyRows)))


  out <- new(Class = "schema",
             clusters = clusters,
             format = schema@format,
             filter = filter,
             variables = variables)

  return(out)

}
