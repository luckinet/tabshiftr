#' Check schema description for consistency
#'
#' This function takes an input table and a respective input schema. It
#' evaluates all \code{\link{find}} specifications in the scope of the input
#' table and ensures that the schema is fomally consistent.
#' @param input an input for which to check a schema description.
#' @param schema the schema description.
#' @return An updated schema description that is formally consistent and has all
#'   .find specs resolved into absolute positions.
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
  } else if(is_quosure(clusters$row)){
    term <- eval_tidy(clusters$row)
    rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
      grepl(x = paste(input[ix,], collapse = " "), pattern = term)
    })
    clusters$row <- which(rows == 1)
  }

  if(is.null(clusters$col)){
    clusters$col <- 1
  } else if(is_quosure(clusters$col)){
    term <- eval_tidy(clusters$col)
    cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
      grepl(x = paste(input[[ix]], collapse = " "), pattern = term)
    })
    clusters$col <- which(cols == 1)
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
  if(!filter$invert){
    if(!is.null(filter$row)){
      filter$row <- (1:tabDim[1])[-filter$row]
    }
  }

  if(is_quosure(filter$row)){
    term <- eval_tidy(filter$row)
    if(is.function(term)){
      map_int(.x = 1:dim(input)[1], .f = function(ix){
        grepl(x = paste(input[ix,], collapse = " "), pattern = term)
      })
    } else {
      rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
        grepl(x = paste(input[ix,], collapse = " "), pattern = term)
      })
      filter$row <- which(rows == 1)
    }
  }
  topAfterFilter <- min(which(!1:dim(input)[1] %in% filter$row))

  # 3. complete variables ----
  outsideCluster <- NULL
  clusterID <- clusters$id
  groupID <- clusters$group
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
    if(is_quosure(varProp$row)){
      term <- eval_tidy(varProp$row)

      if(is.function(term)){

        if(!is.null(varProp$col)){
          subset <- input[,unique(varProp$col)]
        } else {
          subset <- input
        }

        # make a subset table that contains numbers when possible
        subset <- subset %>%
          mutate(across(everything(), function(x) replace_na(x, 0))) %>%
          mutate(across(.cols = where(function(x) suppressWarnings(!anyNA(as.numeric(x)))), .fns = as.numeric))

        rows <- map_lgl(.x = 1:dim(input)[1], .f = function(ix){
          map(subset[x,], term)[[1]]
        })

      } else {
        rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
          grepl(x = paste(input[ix,], collapse = " "), pattern = term)
        })
      }

      varProp$row <- which(rows == 1)
    }

    if(is_quosure(varProp$col)){

      term <- eval_tidy(varProp$col)

      if(is.function(term)){

        if(!is.null(varProp$row)){
          subset <- input[unique(varProp$row),]
        } else {
          subset <- input
        }

        # make a subset table that contains numbers when possible
        subset <- subset %>%
          mutate(across(everything(), function(x) replace_na(x, 0))) %>%
          mutate(across(.cols = where(function(x) suppressWarnings(!anyNA(as.numeric(x)))), .fns = as.numeric))

        cols <- map_lgl(.x = 1:dim(input)[2], .f = function(ix){
          map(subset[[ix]], term)[[1]]
        })

      } else {
        cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
          grepl(x = paste(input[[ix]], collapse = " "), pattern = term)
        })
      }

      varProp$col <- which(cols == 1)
    }

    # figure our which rows to filter out
    if(!varProp$dist){
      if(varProp$type == "observed"){
        if(is.null(varProp$row)){
          varProp$row <- clusters$row
        }
        if(any(varProp$row < topAfterFilter)){
          varProp$row[which(varProp$row < topAfterFilter)] <- topAfterFilter
        }
      }
      filter$row <- sort(unique(c(filter$row, varProp$row)))
    }

    # check whether the variable is actually distinct (i.e., outside of clusters) ----
    if(!varName %in% c(clusterID, groupID)){
      if(all(varProp$col < clusters$col)){
        varProp$dist <- TRUE
      }
    }
    if(varProp$type == "id" & !is.null(varProp$val)){
      varProp$dist <- TRUE
    }

    # make sure that all elements occur the same number of times
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

    variables[[i]] <- varProp
    names(variables)[i] <- varName
  }

  # 4. remove empty rows ----
  emptyRows <- which(rowSums(is.na(input)) == ncol(input))
  filter$row <- sort(unique(c(filter$row, emptyRows)))

  out <- new(Class = "schema",
             clusters = clusters,
             format = schema@format,
             filter = filter,
             variables = variables)

  return(out)

}
