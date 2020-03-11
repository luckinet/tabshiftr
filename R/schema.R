#' The \code{schema} class (S4) and its methods
#'
#' A \code{schema} stores the information of where which information is tored in
#' a table of data.
#' @slot cluster [\code{named list(6)}]\cr metadata of clusters of data
#' @slot variables [\code{named list(.)}]\cr metadata of variables
#' @details The slot \code{variables} typically contains several lists that each
#' record the metadata of the respective variables.
#' @importFrom rlang is_integerish
#' @importFrom stringr str_sub

schema <- setClass(Class = "schema",
                   slots = c(clusters = "list",
                             header = "list",
                             meta = "list",
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
    if(!all(names(object@clusters) %in% c("top", "left", "width", "height", "id"))){
      errors <- c(errors, "'names(schema$clusters)' must be a permutation of set {top,left,width,height,id}")
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
  }

  if(!.hasSlot(object = object, name = "header")){
    errors <- c(errors, "the schema does not have a 'header' slot.")
  } else {
    if(!is.list(object@header)){
      errors <- c(errors, "the slot 'header' is not a list.")
    }
    if(length(object@header) == 0){
      errors <- c(errors, "the slot 'header' does not contain any entries.")
    }
    if(!all(c("row", "rel") %in% names(object@header))){
      errors <- c(errors, "'names(schema$header)' must be a permutation of set {row,rel}")
    }
    if(!is.null(object@header$row)){
      if(!is.numeric(object@header$row)){
        errors <- c(errors, "'schema$header$row' must have a numeric value.")
      }
    }
    if(!is.null(object@header$rel)){
      if(!is.logical(object@header$rel)){
        errors <- c(errors, "'schema$header$rel' must have a logical value.")
      }
    }
  }

  if(!.hasSlot(object = object, name = "meta")){
    errors <- c(errors, "the schema does not have a 'meta' slot.")
  } else {
    if(!is.list(object@meta)){
      errors <- c(errors, "the slot 'meta' is not a list.")
    }
    if(length(object@meta) == 0){
      errors <- c(errors, "the slot 'meta' does not contain any entries.")
    }
    if(!all(names(object@meta) %in% c("del", "dec", "na", "types"))){
      errors <- c(errors, "'names(schema$meta)' must be a permutation of set {del,dec,na,types}")
    }
    if(!is.null(object@meta$del)){
      if(!is.character(object@meta$del)){
        errors <- c(errors, "'schema$meta$del' must have a character value.")
      }
    }
    if(!is.null(object@meta$dec)){
      if(!is.character(object@meta$dec)){
        errors <- c(errors, "'schema$meta$dec' must have a character value.")
      }
    }
    if(!is.null(object@meta$na)){
      if(!is.character(object@meta$na)){
        errors <- c(errors, "'schema$meta$na' must have a character value.")
      }
    }
    if(!is.null(object@meta$types)){
      if(!is.character(object@meta$types)){
        errors <- c(errors, "'schema$meta$types' must have a character value.")
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

      if(!theVariable$type %in% c("id", "measured")){
        errors <- c(errors, paste0("the variables '", theName, "' does must be of type 'id' or 'measured'."))
        return(paste0("\n", errors))
      }

      if(theVariable$type == "id"){
        if(!all(names(theVariable) %in% c("type", "value", "split", "row", "col", "rel", "dist"))){
          errors <- c(errors, paste0("'names(", theName, ")' must be a permutation of set {type,value,split,row,col,rel,dist}"))
        }
        if(!is.null(theVariable$value)){
          if(!is.character(theVariable$value)){
            errors <- c(errors, paste0("'", theName, "$value' must have a character value."))
          }
        }
        if(!is.null(theVariable$split)){
          if(!is.character(theVariable$split)){
            errors <- c(errors, paste0("'", theName, "$split' must have a character value."))
          }
        }
        if(!is.null(theVariable$row)){
          if(!is.numeric(theVariable$row)){
            errors <- c(errors, paste0("'", theName, "$row' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$col)){
          if(!is.numeric(theVariable$col)){
            errors <- c(errors, paste0("'", theName, "$col' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$rel)){
          if(!is.logical(theVariable$rel)){
            errors <- c(errors, paste0("'", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
          }
        }
        if(!is.null(theVariable$dist)){
          if(!is.logical(theVariable$dist)){
            errors <- c(errors, paste0("'", theName, "$dist' must either be 'TRUE' or 'FALSE'."))
          }
        }

      } else {
        if(!all(names(theVariable) %in% c("type", "unit", "factor", "row", "col", "rel", "dist", "key", "value"))){
          errors <- c(errors, paste0("'names(", theName, ")' must be a permutation of set {type,unit,factor,row,col,rel,dist,key,value}"))
        }
        if(!is.null(theVariable$unit)){
          if(!is.character(theVariable$unit)){
            errors <- c(errors, paste0("'", theName, "$unit' must have a character value."))
          }
        }
        if(!is.null(theVariable$factor)){
          if(!is.numeric(theVariable$factor)){
            errors <- c(errors, paste0("'", theName, "$factor' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$row)){
          if(!is.numeric(theVariable$row)){
            errors <- c(errors, paste0("'", theName, "$row' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$col)){
          if(!is.numeric(theVariable$col)){
            errors <- c(errors, paste0("'", theName, "$col' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$rel)){
          if(!is.logical(theVariable$rel)){
            errors <- c(errors, paste0("'", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
          }
        }
        if(!is.null(theVariable$dist)){
          if(!is.logical(theVariable$dist)){
            errors <- c(errors, paste0("'", theName, "$dist' must either be 'TRUE' or 'FALSE'."))
          }
        }
        if(!is.null(theVariable$key)){
          if(!is.character(theVariable$key)){
            errors <- c(errors, paste0("'", theName, "$key' must have a character value."))
          }
        }
        if(!is.null(theVariable$value)){
          if(theVariable$key == "cluster"){
            if(!rlang::is_integerish(theVariable$value)){
              errors <- c(errors, paste0("'", theName, "$value' must have an integer value."))
            }
          } else {
            if(!is.character(theVariable$value)){
              errors <- c(errors, paste0("'", theName, "$value' must have a character value."))
            }
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
              if(is.null(clusters$left)){
                left <- 1
              } else {
                left <- clusters$left
              }
              if(is.null(clusters$top)){
                top <- 1
              } else {
                top <- clusters$top
              }
              clusterSpecs <- paste0("\n    origin: ", paste(top, left, collapse = ", ", sep = "|"), "  (top|left)",
                                     ifelse(!is.null(clusters$id), paste0("\n    id    : ", clusters$id), ""))
            }
            cat(paste0("  ", nClusters, " ", nClustName, clusterSpecs, "\n\n"))

            theNames <- sapply(seq_along(variables), function(x){
              names(variables)[x]
            })
            nNames <- sapply(seq_along(theNames), function(x){
              ifelse(test = is.null(theNames[[x]]) , yes = 0, no = nchar(theNames[x]))
            })
            maxNames <- ifelse(any(nNames > 8), max(nNames), 8)
            theTypes <- sapply(seq_along(variables), function(x){
              variables[[x]]$type
            })
            theRows <- sapply(seq_along(variables), function(x){
              if(is.null(variables[[x]]$row)){
                ""
              } else {
                temp <- unique(variables[[x]]$row)
                # make a short sequence of 'theRows'
                dists <- temp - c(temp[1]-1, temp)[-(length(temp)+1)]
                if(all(dists == 1) & length(temp) > 1){
                  paste0(min(temp), ":", max(temp))
                } else {
                  temp
                }
              }
            })
            nRow <- sapply(seq_along(theRows), function(x){
              ifelse(test = is.null(theRows[[x]]) , yes = 0, no = nchar(paste0(theRows[[x]], collapse = ", ")))
            })
            maxRows <- ifelse(any(nRow > 3), max(nRow), 3)
            theCols <- sapply(seq_along(variables), function(x){
              if(is.null(variables[[x]]$col)){
                ""
              } else {
                temp <- unique(variables[[x]]$col)
                # make a short sequence of 'theRows'
                dists <- temp - c(temp[1]-1, temp)[-(length(temp)+1)]
                if(all(dists == 1) & length(temp) > 1){
                  paste0(min(temp), ":", max(temp))
                } else {
                  temp
                }
              }
            })
            nCols <- sapply(seq_along(theCols), function(x){
              ifelse(test = is.null(theCols[[x]]) , yes = 0, no = nchar(paste0(theCols[[x]], collapse = ", ")))
            })
            maxCols <- ifelse(any(nCols > 3), max(nCols), 3)
            theKeys <- sapply(seq_along(variables), function(x){
              if(variables[[x]]$type == "id"){
                NULL
              } else {
                variables[[x]]$key
              }
            })
            nKeys <- sapply(seq_along(theKeys), function(x){
              ifelse(test = is.null(theKeys[[x]]) , yes = 0, no = nchar(theKeys[x]))
            })
            maxKeys <- ifelse(any(nKeys > 3), max(nKeys), 3)
            theValues <- sapply(seq_along(variables), function(x){
              if(variables[[x]]$type == "id"){
                NULL
              } else {
                variables[[x]]$value
              }
            })
            nVals <- sapply(seq_along(theValues), function(x){
              ifelse(test = is.null(theValues[[x]]) , yes = 0, no = nchar(theValues[x]))
            })
            maxVals <- ifelse(any(nVals > 5), max(nVals), 5)
            theRels <- sapply(seq_along(variables), function(x){
              str_sub(as.character(variables[[x]]$rel), 1, 1)
            })

            for(i in 1:(length(variables)+1)){

              if(i == 1){
                whiteSpace1 <- paste0(rep(" ", times = maxNames+2-8+1), collapse = "")
                whiteSpace2 <- "     "
                whiteSpace3 <- paste0(rep(" ", times = maxRows), collapse = "")
                whiteSpace4 <- paste0(rep(" ", times = maxCols), collapse = "")
                whiteSpace5 <- paste0(rep(" ", times = maxKeys), collapse = "")
                whiteSpace6 <- paste0(rep(" ", times = maxVals-2), collapse = "")

                cat(paste0("   ", "variable", whiteSpace1,
                           "type", whiteSpace2,
                           "row", whiteSpace3,
                           "col", whiteSpace4,
                           "key", whiteSpace5,
                           "value", whiteSpace6,
                           "rel",
                           "\n"))
                cat(" ", paste(paste0(rep("-", maxNames+2), collapse = ""),
                               paste0(rep("-", 8), collapse = ""),
                               paste0(rep("-", maxRows+2), collapse = ""),
                               paste0(rep("-", maxCols+2), collapse = ""),
                               paste0(rep("-", maxKeys+2), collapse = ""),
                               paste0(rep("-", maxVals+2), collapse = ""),
                               paste0(rep("-", 5), collapse = ""), collapse = " "), "\n")
              } else {
                whiteSpace1 <- paste0(rep(" ", times = maxNames+2-nchar(theNames[[i-1]])+1), collapse = "")
                whiteSpace2 <- ifelse(theTypes[[i-1]] == "id", "       ", "   ")
                whiteSpace3 <- paste0(rep(" ", times = maxRows+2-nRow[[i-1]]+1), collapse = "")
                whiteSpace4 <- paste0(rep(" ", times = maxCols+2-nCols[[i-1]]+1), collapse = "")
                whiteSpace5 <- paste0(rep(" ", times = maxKeys+2-nKeys[[i-1]]+1), collapse = "")
                whiteSpace6 <- paste0(rep(" ", times = maxVals+2-nVals[[i-1]]+1), collapse = "")

                cat(paste0("   ", yellow(theNames[[i-1]]), whiteSpace1,
                           theTypes[[i-1]], whiteSpace2,
                           paste0(theRows[[i-1]], collapse = ", "), whiteSpace3,
                           paste0(theCols[[i-1]], collapse = ", "), whiteSpace4,
                           theKeys[[i-1]], whiteSpace5,
                           theValues[[i-1]], whiteSpace6,
                           theRels[[i-1]], "  ",
                           "\n"))
              }


            }

          })
