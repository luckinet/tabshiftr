#' Rectangrise a table
#'
#' This function takes the output from \code{\link{register}} and rearranges
#' columns and rows so that the resulting table has a perfectly rectangular
#' format.
#' @param input [\code{data.frame(1)}]\cr table to rectangularise.
#' @importFrom checkmate assertDataFrame
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows bind_cols rename
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @export

reorganise <- function(input = NULL){

  # check validity of arguments
  assertDataFrame(x = input)

  # check whether there is already a metadata object
  if(!exists(x = "dmt_current", envir = baseenv())){
    stop("please first use 'register()' to specify the table properties.")
  } else{
    current <- get(x = "dmt_current", envir = baseenv())
    default <- get(x = "dmt_default", envir = baseenv())
  }

  # derive the full configuration for clusters
  clusters <- current$body$clusters
  nClusters <- max(lengths(clusters))
  ### set width and height if they are NULL
  if(is.null(clusters$width)){
    clusters$width <- dim(input)[2] - min(clusters$col)
  }
  if(is.null(clusters$height)){
    clusters$height <- dim(input)[1] - min(clusters$row)
  }
  ### make sure that all elements occur the same number of times
  clusters$row <- rep(x = clusters$row, length.out = nClusters)
  clusters$col <- rep(x = clusters$col, length.out = nClusters)
  clusters$width <- rep(x = clusters$width, length.out = nClusters)
  clusters$height <- rep(x = clusters$height, length.out = nClusters)

  # create empty tibbles for row binding
  varNames <- unlist(lapply(seq_along(current$variables), function(x){
    vals <- current$variables[[x]]
    if(!all(c(is.null(vals$row), is.null(vals$col)))){
      return(names(current$variables)[x])
    }
  }))

  values <- lapply(seq_along(varNames), function(x){
    tibble(.rows = 0)
  })
  # go through all clusters and process them ...
  for(i in 1:nClusters){
    data <- input[clusters$row[i]:(clusters$row[i]+clusters$height),
                  clusters$col[i]:(clusters$col[i]+clusters$width)]

    # cycle through all variables of interest
    tempValues <- list()
    newNames <- NULL
    oldNames <- varNames
    for(j in seq_along(varNames)){
      theVariable <- varNames[j]
      var <- current$variables[[theVariable]]

      # test whether the variable is registered at all.
      if(all(c(is.null(var$row), is.null(var$col)))){
        stop(paste0("please register ", theVariable, "."))
      }

      # determine whether the variable is either 'key' or 'values'
      isKey <- FALSE
      isValues <- FALSE
      if(all(!is.null(var$row), !is.null(current$variables$key$row))){
        if(var$row == current$variables$key$row){
          isKey <- TRUE
        }
      }
      if(all(!is.null(var$col), !is.null(current$variables$key$col))){
        if(var$col == current$variables$key$col){
          isKey <- TRUE
        }
      }
      if(all(!is.null(var$row), !is.null(current$variables$values$row))){
        if(var$row == current$variables$values$row){
          isValues <- TRUE
        }
      }
      if(all(!is.null(var$col), !is.null(current$variables$values$col))){
        if(var$col == current$variables$values$col){
          isValues <- TRUE
        }
      }

      # replace the variable name with 'value', if it's not 'values' itself
      if(!is.null(var$value) & !isValues){
        oldNames[j] <- var$value
        theVariable <- var$value
      }

      # proceed with extracting this variable only if it's not already covered
      # by 'values', which is true either when one of them is NULL, or when they
      # both have a different value other than NULL
      if(!is.null(var$row) & !theVariable %in% c("key", "values")){
        if(isValues){
          oldNames <- oldNames[-which(oldNames == theVariable)]
          if(!is.null(var$value)){
            newName <- setNames(object = var$value, nm = theVariable)
            newNames <- c(newNames, newName)
          }
          next
        }
      }
      if(!is.null(var$col) & !theVariable %in% c("key", "values")){
        if(isValues){
          oldNames <- oldNames[-which(oldNames == theVariable)]
          if(!is.null(var$value)){
            newName <- setNames(object = var$value, nm = theVariable)
            newNames <- c(newNames, newName)
          }
          next
        }
      }

      # get the values
      if(is.null(var$row)){
        temp <- data[, var$col]
      } else if(is.null(var$col)){
        temp <- data[var$row, ]
      } else{
        temp <- data[var$row, var$col]
      }
      tempValues <- c(tempValues, list(temp))

      # include copies of the current variable name when there is more than one
      # output
      if(dim(temp)[2] > 1){
        before <- oldNames[0:(which(oldNames == theVariable)-1)]
        after <- oldNames[(which(oldNames == theVariable)+1):length(oldNames)]
        self <- paste0(theVariable, 1:dim(temp)[2])
        oldNames <- c(before, self, after)
      }
    }

    # row bind the values of all clusters
    values <- lapply(seq_along(tempValues), function(x){
      bind_rows(values[[x]], tempValues[[x]])
    })

  }
  out <- tibble(.rows = dim(values[[1]])[1])
  for(i in seq_along(values)){
    out <- bind_cols(out, values[[i]])
  }
  colnames(out) <- oldNames

  if(current$body$format$type == "long"){
    out <- out %>%
      spread(key = key, value = values, convert = TRUE) %>%
      rename(!!newNames)
  }

  return(out)
}