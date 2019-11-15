#' The \code{schema} class (S4) and its methods
#'
#' A \code{schema} stores the information of where which information is tored in
#' a table of data.
#' @slot cluster [\code{named list(6)}]\cr metadata of clusters of data
#' @slot variables [\code{named list(.)}]\cr metadata of variables
#' @details The slot \code{variables} typically contains several lists that each
#' record the metadata of the respective variables.

schema <- setClass(Class = "schema",
                   slots = c(clusters = "list",
                             variables = "list"
                   )
)

setValidity(Class = "schema", function(object){

  errors <- character()

  if(!.hasSlot(object = object, name = "clusters")){
    errors <- c(errors, "the schema does not have a 'clusters' slot.")
  } else {
    if(!is.list(object@clusters)){
      errors <- c(errors, "the slot 'clusters' is not a list.")
    }
    if(!all(c("top", "left", "width", "height", "id", "header") %in% names(object@clusters))){
      errors <- c(errors, "'names(schema$clusters)' must be a permutation of set {top,left,width,height,id,header}")
    }
    if(!is.null(object@clusters$top)){
      if(!is.numeric(object@clusters$top)){
        errors <- c(errors, "'schema$clusters$top' must have a numeric value.")
      }
    }
    if(!is.null(object@clusters$left)){
      if(!is.numeric(object@clusters$left)){
        errors <- c(errors, "'schema$clusters$left' must have a numeric value.")
      }
    }
    if(!is.null(object@clusters$width)){
      if(!is.numeric(object@clusters$width)){
        errors <- c(errors, "'schema$clusters$width' must have a numeric value.")
      }
    }
    if(!is.null(object@clusters$height)){
      if(!is.numeric(object@clusters$height)){
        errors <- c(errors, "'schema$clusters$height' must have a numeric value.")
      }
    }
    if(!is.null(object@clusters$id)){
      if(!is.character(object@clusters$id)){
        errors <- c(errors, "'schema$clusters$id' must have a character value.")
      }
    }
    if(!is.null(object@clusters$header)){
      if(!is.logical(object@clusters$header)){
        errors <- c(errors, "'schema$clusters$header' must either be 'TRUE' or 'FALSE'.")
      }
    }
  }

  if(!.hasSlot(object = object, name = "variables")){
    errors <- c(errors, "the schema does not have a 'variables' slot.")
  } else {
    if(!is.list(object@variables)){
      errors <- c(errors, "the slot 'variables' is not a list.")
    }
    if(length(object@variables) == 0){
      errors <- c(errors, "the slot 'variables' does not contain any entries.")
    }
    for(i in seq_along(object@variables)){
      theVariable <- object@variables[[i]]
      theName <- names(object@variables)[i]

      if(!theVariable$type %in% c("id", "values")){
        errors <- c(errors, paste0("the variables '", theName, "' does must be of type 'id' or 'values'."))
        return(paste0("\n", errors))
      }

      if(theVariable$type == "id"){
        if(!all(c("type", "name", "split", "row", "col", "rel") %in% names(theVariable))){
          errors <- c(errors, paste0("'names(schema$variables$", theName, ")' must be a permutation of set {type,name,split,row,col,rel}"))
        }
        if(!is.null(theVariable$name)){
          if(!is.character(theVariable$name)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$name' must have a character value."))
          }
        }
        if(!is.null(theVariable$split)){
          if(!is.character(theVariable$split)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$split' must have a character value."))
          }
        }
        if(!is.null(theVariable$row)){
          if(!is.numeric(theVariable$row)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$row' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$col)){
          if(!is.numeric(theVariable$col)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$col' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$rel)){
          if(!is.logical(theVariable$rel)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
          }
        }

      } else {
        if(!all(c("type", "unit", "factor", "row", "col", "rel", "key", "value") %in% names(theVariable))){
          errors <- c(errors, paste0("'names(schema$variables$", theName, ")' must be a permutation of set {type,unit,factor,row,col,rel,key,value}"))
        }
        if(!is.null(theVariable$unit)){
          if(!is.character(theVariable$unit)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$unit' must have a character value."))
          }
        }
        if(!is.null(theVariable$factor)){
          if(!is.numeric(theVariable$factor)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$factor' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$row)){
          if(!is.numeric(theVariable$row)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$row' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$col)){
          if(!is.numeric(theVariable$col)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$col' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$rel)){
          if(!is.logical(theVariable$rel)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
          }
        }
        if(!is.null(theVariable$key)){
          if(!is.character(theVariable$key)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$key' must have a character value."))
          }
        }
        if(!is.null(theVariable$value)){
          if(!is.character(theVariable$value)){
            errors <- c(errors, paste0("'schema$variables$", theName, "$value' must have a character value."))
          }
        }

      }

    }
  }

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(paste0("\n", errors))
  }
})

#' Print the \code{schema}
#'
#' @param object [\code{schema}]\cr the schema to print.
#' @importFrom crayon yellow

setMethod(f = "show",
          signature = "schema",
          definition = function(object){
            clusters <- object@clusters
            variables <- object@variables

            nClusters <- ifelse(length(clusters$top) == 0, 1, length(clusters$top))
            nvars <- length(variables)
            theNames <- names(variables)
            nClustName <- ifelse(nClusters > 1, "clusters", "cluster")

            # cat(paste0("Schema description of ", length(object@variables), " variables.\n"))
            # cat("\n")
            if(is.null(clusters$top) & is.null(clusters$left) & is.null(clusters$width) & is.null(clusters$height)){
              clusterSpecs <- paste0(" (whole spreadsheet)")
            } else {
              left <- ifelse(is.null(clusters$left), 1, clusters$left)
              top <- ifelse(is.null(clusters$top), 1, clusters$top)
              clusterSpecs <- paste0("\n    origin: ", top, ",", left, " (top,left)",
                                     ifelse(!is.null(clusters$id), paste0("\n    id    : ", clusters$id), ""))
            }
            cat(paste0("  ", nClusters, " ", nClustName, clusterSpecs, "\n\n"))

            theNames <- sapply(seq_along(variables), function(x){
              names(variables)[x]
            })
            nNames <- nchar(theNames)
            maxNames <- ifelse(any(nNames > 9), max(nNames), 9)
            theTypes <- sapply(seq_along(variables), function(x){
              variables[[x]]$type
            })
            theRows <- sapply(seq_along(variables), function(x){
              if(is.null(variables[[x]]$row)){
                ""
              } else {
                variables[[x]]$row
              }
            })
            nRow <- nchar(theRows)
            maxRows <- ifelse(any(nRow > 3), max(nRow), 3)
            theCols <- sapply(seq_along(variables), function(x){
              if(is.null(variables[[x]]$col)){
                ""
              } else {
                paste0(variables[[x]]$col, collapse = ", ")
              }
            })
            nCols <- nchar(theCols)
            maxCols <- ifelse(any(nCols > 4), max(nCols), 4)
            theKeys <- sapply(seq_along(variables), function(x){
              if(variables[[x]]$type == "id"){
                NULL
              } else {
                variables[[x]]$key
              }
            })
            nKeys <- nchar(theKeys)
            maxKeys <- ifelse(any(nKeys > 4), max(nKeys), 4)
            theValues <- sapply(seq_along(variables), function(x){
              if(variables[[x]]$type == "id"){
                NULL
              } else {
                variables[[x]]$value
              }
            })
            nVals <- nchar(theValues)
            maxVals <- ifelse(any(nVals > 5), max(nVals), 5)

            for(i in 1:(length(variables)+1)){

              if(i == 1){
                whiteSpace1 <- paste0(rep(" ", times = maxNames-8+1), collapse = "")
                whiteSpace2 <- "   "
                whiteSpace3 <- paste0(rep(" ", times = maxRows-2), collapse = "")
                whiteSpace4 <- paste0(rep(" ", times = maxCols-2), collapse = "")
                whiteSpace5 <- paste0(rep(" ", times = maxKeys-2), collapse = "")
                whiteSpace6 <- paste0(rep(" ", times = maxVals-2), collapse = "")

                cat(paste0("  ", "variable", whiteSpace1,
                           "type", whiteSpace2,
                           "row", whiteSpace3,
                           "col", whiteSpace4,
                           "key", whiteSpace5,
                           "value", whiteSpace6,
                           "\n"))
                cat(" ", paste(paste0(rep("-", maxNames), collapse = ""),
                               paste0(rep("-", 6), collapse = ""),
                               paste0(rep("-", maxRows), collapse = ""),
                               paste0(rep("-", maxCols), collapse = ""),
                               paste0(rep("-", maxKeys), collapse = ""),
                               paste0(rep("-", maxVals), collapse = ""), collapse = " "), "\n")
              } else {
                whiteSpace1 <- paste0(rep(" ", times = maxNames-nchar(theNames[[i-1]])+1), collapse = "")
                whiteSpace2 <- ifelse(theTypes[[i-1]] == "id", "     ", " ")
                whiteSpace3 <- paste0(rep(" ", times = maxRows-nRow[[i-1]]+1), collapse = "")
                whiteSpace4 <- paste0(rep(" ", times = maxCols-nCols[[i-1]]+1), collapse = "")
                whiteSpace5 <- paste0(rep(" ", times = maxKeys-nKeys[[i-1]]+1), collapse = "")
                whiteSpace6 <- paste0(rep(" ", times = maxVals-nVals[[i-1]]+1), collapse = "")

                cat(paste0("  ", yellow(theNames[[i-1]]), whiteSpace1,
                           theTypes[[i-1]], whiteSpace2,
                           paste0(theRows[[i-1]], collapse = ", "), whiteSpace3,
                           paste0(theCols[[i-1]], collapse = ", "), whiteSpace4,
                           theKeys[[i-1]], whiteSpace5,
                           theValues[[i-1]], whiteSpace6,
                           "\n"))
              }


            }

          })
