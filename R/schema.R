#' The \code{schema} class (S4) and its methods
#'
#' A \code{schema} stores the information of where which information is stored in
#' a table of data.
#' @slot cluster [\code{list(1)}]\cr description of clusters of data.
#' @slot header [code{list(1)}]\cr description of the header.
#' @slot meta [\code{list(1)}]\cr description of the metadata.
#' @slot variables [\code{named list(.)}]\cr description of variables.
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
    if(!all(names(object@clusters) %in% c("row", "col", "width", "height", "id"))){
      errors <- c(errors, "'names(schema$clusters)' must be a permutation of set {row,col,width,height,id}")
    }
    if(!is.null(object@clusters$row)){
      if(!is.numeric(object@clusters$row)){
        errors <- c(errors, "'schema$clusters$row' must have a numeric value.")
      }
    }
    if(!is.null(object@clusters$col)){
      if(!is.numeric(object@clusters$col)){
        errors <- c(errors, "'schema$clusters$col' must have a numeric value.")
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
    if(!all(names(object@header) %in% c("row", "rel", "merge"))){
      errors <- c(errors, "'names(header)' must be a permutation of set {row,rel,merge}")
    }
    if(!is.null(object@header$row)){
      if(!is.numeric(object@header$row)){
        errors <- c(errors, "'header$row' must have a numeric value.")
      }
    }
    if(!is.logical(object@header$rel)){
      errors <- c(errors, "'header$rel' must have a logical value.")
    }
    if(!is.logical(object@header$merge)){
      errors <- c(errors, "'header$merge' must have a logical value.")
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
    if(!all(names(object@meta) %in% c("del", "dec", "na"))){
      errors <- c(errors, "'names(schema$meta)' must be a permutation of set {del,dec,na}")
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
    # if(!is.null(object@meta$types)){
    #   if(!is.character(object@meta$types)){
    #     errors <- c(errors, "'schema$meta$types' must have a character value.")
    #   }
    # }
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
        if(!is.logical(theVariable$rel)){
          errors <- c(errors, paste0("'", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
        }
        if(!is.logical(theVariable$dist)){
          errors <- c(errors, paste0("'", theName, "$dist' must either be 'TRUE' or 'FALSE'."))
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
        if(!is.logical(theVariable$rel)){
          errors <- c(errors, paste0("'", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
        }
        if(!is.logical(theVariable$dist)){
          errors <- c(errors, paste0("'", theName, "$dist' must either be 'TRUE' or 'FALSE'."))
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
#' @importFrom stringr str_split

setMethod(f = "show",
          signature = "schema",
          definition = function(object){
            clusters <- object@clusters
            variables <- object@variables

            nClusters <- ifelse(length(clusters$row) == 0, 1, length(clusters$row))
            nvars <- length(variables)
            theNames <- names(variables)
            nClustName <- ifelse(nClusters > 1, "clusters", "cluster")

            # make and print cluster info ----
            if(is.null(clusters$row) & is.null(clusters$col) & is.null(clusters$width) & is.null(clusters$height)){
              clusterSpecs <- paste0(" (whole spreadsheet)")
            } else {
              if(is.null(clusters$col)){
                left <- 1
              } else {
                left <- clusters$col
              }
              if(is.null(clusters$row)){
                top <- 1
              } else {
                top <- clusters$row
              }
              clusterSpecs <- paste0("\n    origin: ", paste(top, left, collapse = ", ", sep = "|"), "  (row|col)",
                                     ifelse(!is.null(clusters$id), paste0("\n    id    : ", clusters$id), ""))
            }
            cat(paste0("  ", nClusters, " ", nClustName, clusterSpecs, "\n\n"))

            # make and print variable info ----
            included <- c(TRUE, TRUE)
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

            # rows
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
            if(any(nRow != 0)){
              included <- c(included, TRUE)
            } else {
              included <- c(included, FALSE)
            }

            # columns
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
            if(any(nCols != 0)){
              included <- c(included, TRUE)
            } else {
              included <- c(included, FALSE)
            }

            # keys
            theKeys <- sapply(seq_along(variables), function(x){
              if(variables[[x]]$type == "id"){
                NULL
              } else {
                if(!is.null(variables[[x]]$key)){
                  if(grepl(pattern = "\n", variables[[x]]$key)){
                    paste0(str_split(string = variables[[x]]$key, pattern = "\n", simplify = TRUE)[1], " ...")
                  } else {
                    variables[[x]]$key
                  }
                } else {
                  NULL
                }

              }
            })
            nKeys <- sapply(seq_along(theKeys), function(x){
              ifelse(test = is.null(theKeys[[x]]) , yes = 0, no = nchar(theKeys[x]))
            })
            maxKeys <- ifelse(any(nKeys > 3), max(nKeys), 3)
            if(any(nKeys != 0)){
              included <- c(included, TRUE)
            } else {
              included <- c(included, FALSE)
            }

            # values
            theValues <- sapply(seq_along(variables), function(x){
              if(variables[[x]]$type == "id"){
                NULL
              } else {
                if(!is.null(variables[[x]]$value)){
                  if(grepl(pattern = "\n", variables[[x]]$value)){
                    paste0(str_split(string = variables[[x]]$value, pattern = "\n", simplify = TRUE)[1], " ...")
                  } else {
                    variables[[x]]$value
                  }
                } else {
                  NULL
                }
              }
            })
            nVals <- sapply(seq_along(theValues), function(x){
              ifelse(test = is.null(theValues[[x]]) , yes = 0, no = nchar(theValues[x]))
            })
            maxVals <- ifelse(any(nVals > 5), max(nVals), 5)
            if(any(nVals != 0)){
              included <- c(included, TRUE)
            } else {
              included <- c(included, FALSE)
            }

            # whether variables are relative
            theRels <- sapply(seq_along(variables), function(x){
              str_sub(as.character(variables[[x]]$rel), 1, 1)
            })
            included <- c(included, TRUE)

            # whether variables are distinct
            theDist <- sapply(seq_along(variables), function(x){
              str_sub(as.character(variables[[x]]$dist), 1, 1)
            })
            included <- c(included, TRUE)

            for(i in 1:(length(variables)+1)){

              if(i == 1){

                head1 <- paste0("   ", "variable", paste0(rep(" ", times = maxNames-5), collapse = ""))
                line1 <- paste0(c(rep("-", maxNames+2), " "), collapse = "")
                head2 <- paste0("type       ")
                line2 <- paste0(c(rep("-", 10), " "), collapse = "")
                if(included[3]){
                  head3 <- paste0("row", paste0(rep(" ", times = maxRows), collapse = ""))
                  line3 <- paste0(c(rep("-", maxRows+2), " "), collapse = "")
                } else {
                  head3 <- line3 <- ""
                }
                if(included[4]){
                  head4 <- paste0("col", paste0(rep(" ", times = maxCols), collapse = ""))
                  line4 <- paste0(c(rep("-", maxCols+2), " "), collapse = "")
                } else {
                  head4 <- line4 <- ""
                }
                if(included[5]){
                  head5 <- paste0("key", paste0(rep(" ", times = maxKeys), collapse = ""))
                  line5 <- paste0(c(rep("-", maxKeys+2), " "), collapse = "")
                } else {
                  head5 <- line5 <- ""
                }
                if(included[6]){
                  head6 <- paste0("value", paste0(rep(" ", times = maxVals-2), collapse = ""))
                  line6 <- paste0(c(rep("-", maxVals+2), " "), collapse = "")
                } else {
                  head6 <- line6 <- ""
                }
                head7 <- paste0("rel   ")
                head8 <- paste0("dist")

                cat(paste0(head1, head2, head3, head4, head5, head6, head7, head8), "\n")
                cat(" ", paste0(line1, line2, line3, line4, line5, line6,
                               paste0(c(rep("-", 5), " "), collapse = ""),
                               paste0(c(rep("-", 6), " "), collapse = "")), "\n")

              } else {

                var1 <- paste0("   ", yellow(theNames[[i-1]]),
                               paste0(rep(" ", times = maxNames+3-nchar(theNames[[i-1]])), collapse = ""))
                var2 <- paste0(theTypes[[i-1]], ifelse(theTypes[[i-1]] == "id", "         ", "   "))
                if(included[3]){
                  var3 <- paste0(paste0(theRows[[i-1]], collapse = ", "),
                                 paste0(rep(" ", times = maxRows+3-nRow[[i-1]]), collapse = ""))
                } else {
                  var3 <- ""
                }
                if(included[4]){
                  var4 <- paste0(paste0(theCols[[i-1]], collapse = ", "),
                                 paste0(rep(" ", times = maxCols+3-nCols[[i-1]]), collapse = ""))
                } else {
                  var4 <- ""
                }
                if(included[5]){
                  var5 <- paste0(theKeys[[i-1]],
                                 paste0(rep(" ", times = maxKeys+3-nKeys[[i-1]]), collapse = ""))
                } else {
                  var5 <- ""
                }
                if(included[6]){
                  var6 <- paste0(theValues[[i-1]],
                                 paste0(rep(" ", times = maxVals+3-nVals[[i-1]]), collapse = ""))
                } else {
                  var6 <- ""
                }
                var7 <- paste0(theRels[[i-1]], "     ")
                var8 <- paste0(theDist[[i-1]], "  ")

                cat(paste0(var1, var2, var3, var4, var5, var6, var7, var8, "\n"))

              }


            }

          })
