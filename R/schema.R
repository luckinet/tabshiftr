#' The \code{schema} class (S4) and its methods
#'
#' A \code{schema} stores the information of where which information is stored
#' in a table of data.
#' @slot cluster [\code{list(1)}]\cr description of
#'   \code{\link[=setCluster]{clusters}} in the table.
#' @slot format [\code{list(1)}]\cr description of the table
#'   \code{\link[=setFormat]{format}}
#' @slot variables [\code{named list(.)}]\cr description of
#'   \code{\link[=setIDVar]{identifying}} and \code{\link[=setObsVar]{observed}}
#'   variables.
#' @section Setting up schema descriptions: This section outlines the currently
#'   recommended strategy for setting up schema descriptions. For example tables
#'   and the respective schemas, see the vignette.
#'
#'   \enumerate{ \item \emph{Variables}: Clarify which are the identifying
#'   variables and which are the observed variables. Make sure not to mistake a
#'   listed observed variable as identifying variable.
#'
#'   \item \emph{Clusters}: Determine whether there are clusters and if so, find
#'   the origin (top left cell) of each cluster and provide the required
#'   information in \code{\link[=setCluster]{setCluster}(top = ..., left =
#'   ...)}. It is advised to treat a table that contains meta-data in the top
#'   rows as cluster, as this is often the case with implicit variables. All
#'   variables need to be specified in each cluster (in case clusters are all
#'   organised in the same arrangement), or \code{relative = TRUE} can be used.
#'   Data may be organised into clusters a) whenever a set of variables occurs
#'   more than once in the same table, nested into another variable, or b) when
#'   the data are organised into separate spreadsheets or files according to one
#'   of the variables (depending on the context, these issues can also be solved
#'   differently). In both cases the variable responsible for clustering (the
#'   cluster ID) can be either an identifying variable, or a categorical
#'   observed variable: \itemize{
#'
#'   \item in case the cluster ID is an identifying variable, provide its name
#'   in \code{\link[=setCluster]{setCluster(id = ...)}} and specify it as an
#'   identifying variable (\code{\link{setIDVar}})
#'
#'   \item in case it is a observed variable, provide simply
#'   \code{\link[=setCluster]{setCluster}(..., id = "observed")}. }
#'
#'   \item \emph{Meta-data}: Provide potentially information about the format
#'   (\code{\link{setFormat}}).
#'
#'   \item \emph{Identifying variables}: Determine the following: \itemize{
#'
#'   \item is the variable available at all? This is particularly important when
#'   the data are split up into tables that are in spreadsheets or files. Often
#'   the variable that splits up the data (and thus identifies the clusters) is
#'   not explicitly available in the table anymore. In such a case, provide the
#'   value in \code{\link[=setIDVar]{setIDVar}(..., value = ...)}.
#'
#'   \item all columns in which the variable values sit.
#'
#'   \item in case the variable is in several columns, determine additionally
#'   the row in which its values sit. In this case, the values will look like
#'   they are part of a header.
#'
#'   \item in case the variable must be split off of another column, provide a
#'   regular expression that results in the target subset via
#'   \code{\link[=setIDVar]{setIDVar}(..., split = ...)}.
#'
#'   \item in case the variable is distinct from the main table, provide the
#'   explicit (non-relative) position and set
#'   \code{\link[=setIDVar]{setIDVar}(..., distinct = TRUE)}. }
#'
#'   \item \emph{Observed variable}: Determine the following: \itemize{
#'
#'   \item all columns in which the values of the variable sit.
#'
#'   \item the unit and conversion factor.
#'
#'   \item in case the variable is not tidy, go through the following cases one
#'   after the other: \itemize{
#'
#'   \item in case the variable is nested in a wide identifying variable,
#'   determine in addition to the columns in which the values sit also the rows
#'   in which the \emph{variable name} sits.
#'
#'   \item in case the names of the variable are given as a value of an
#'   identifying variable, give the column name as
#'   \code{\link[=setObsVar]{setObsVar}(..., key = ...)}, together with the name
#'   of the respective observed variable (as it appears in the table) in
#'   \code{values}.
#'
#'   \item in case the name of the variable is the ID of clusters, specify
#'   \code{\link[=setObsVar]{setObsVar}(..., key = "cluster", value = ...)},
#'   where \code{values} has the cluster number the variable refers to. } } }
#' @importFrom rlang is_integerish
#' @importFrom stringr str_sub

schema <- setClass(Class = "schema",
                   slots = c(clusters = "list",
                             format = "list",
                             filter = "list",
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
    if(!all(names(object@clusters) %in% c("id", "group", "row", "col", "width", "height", "member"))){
      errors <- c(errors, "'names(schema$clusters)' must be a permutation of set {id,group,row,col,width,height,member}")
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
    if(!is.null(object@clusters$group)){
      if(!is.character(object@clusters$group)){
        errors <- c(errors, "'schema$clusters$group' must have a character value.")
      }
    }
    if(!is.null(object@clusters$member)){
      if(!is.numeric(object@clusters$member)){
        errors <- c(errors, "'schema$clusters$member' must have a numeric value.")
      }
    }
  }

  if(!.hasSlot(object = object, name = "format")){
    errors <- c(errors, "the schema does not have a 'format' slot.")
  } else {
    if(!is.list(object@format)){
      errors <- c(errors, "the slot 'format' is not a list.")
    }
    if(length(object@format) == 0){
      errors <- c(errors, "the slot 'format' does not contain any entries.")
    }
    if(!all(names(object@format) %in% c("del", "dec", "na"))){
      errors <- c(errors, "'names(schema$format)' must be a permutation of set {del,dec,na}")
    }
    if(!is.null(object@format$del)){
      if(!is.character(object@format$del)){
        errors <- c(errors, "'schema$format$del' must have a character value.")
      }
    }
    if(!is.null(object@format$dec)){
      if(!is.character(object@format$dec)){
        errors <- c(errors, "'schema$format$dec' must have a character value.")
      }
    }
    if(!is.null(object@format$na)){
      if(!is.character(object@format$na)){
        errors <- c(errors, "'schema$format$na' must have a character value.")
      }
    }
    # if(!is.null(object@format$types)){
    #   if(!is.character(object@format$types)){
    #     errors <- c(errors, "'schema$format$types' must have a character value.")
    #   }
    # }
  }

  if(!.hasSlot(object = object, name = "filter")){
    errors <- c(errors, "the schema does not have a 'filter' slot.")
  } else {
    if(!is.list(object@filter)){
      errors <- c(errors, "the slot 'filter' is not a list.")
    }
    if(length(object@filter) == 0){
      errors <- c(errors, "the slot 'filter' does not contain any entries.")
    }
    if(!all(names(object@filter) %in% c("row", "col", "invert"))){
      errors <- c(errors, "'names(schema$filter)' must be a permutation of set {row,col,invert}")
    }
    if(!is.null(object@filter$row)){
      if(!is.numeric(object@filter$row)){
        errors <- c(errors, "'schema$filter$row' must have a numeric value.")
      }
    }
    if(!is.null(object@filter$col)){
      if(!is.numeric(object@filter$col)){
        errors <- c(errors, "'schema$filter$col' must have a numeric value.")
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

      if(!theVariable$type %in% c("id", "observed")){
        errors <- c(errors, paste0("the variables '", theName, "' does must be of type 'id' or 'observed'."))
        return(paste0("\n", errors))
      }

      if(theVariable$type == "id"){
        if(!all(names(theVariable) %in% c("type", "value", "row", "col", "rel", "split", "dist", "merge"))){
          errors <- c(errors, paste0("'names(", theName, ")' must be a permutation of set {type,value,row,col,rel,split,merge,dist}"))
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
          if(!(is.numeric(theVariable$row) | testClass(x = theVariable$row, classes = "quosure"))){
            errors <- c(errors, paste0("'", theName, "$row' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$col)){
          if(!(is.numeric(theVariable$col) | testClass(x = theVariable$col, classes = "quosure"))){
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
        # if(!is.null(theVariable$unit)){
        #   if(!is.character(theVariable$unit)){
        #     errors <- c(errors, paste0("'", theName, "$unit' must have a character value."))
        #   }
        # }
        if(!is.null(theVariable$factor)){
          if(!is.numeric(theVariable$factor)){
            errors <- c(errors, paste0("'", theName, "$factor' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$row)){
          if(!(is.numeric(theVariable$row) | testClass(x = theVariable$row, classes = "quosure"))){
            errors <- c(errors, paste0("'", theName, "$row' must have a numeric value."))
          }
        }
        if(!is.null(theVariable$col)){
          if(!(is.numeric(theVariable$col) | testClass(x = theVariable$col, classes = "quosure"))){
            errors <- c(errors, paste0("'", theName, "$col' must have a numeric value."))
          }
        }
        if(!is.logical(theVariable$rel)){
          errors <- c(errors, paste0("'", theName, "$rel' must either be 'TRUE' or 'FALSE'."))
        }
        if(!is.logical(theVariable$dist)){
          errors <- c(errors, paste0("'", theName, "$dist' must either be 'TRUE' or 'FALSE'."))
        }
        # if(!is.null(theVariable$key)){
        #   if(!is.character(theVariable$key)){
        #     errors <- c(errors, paste0("'", theName, "$key' must have a character value."))
        #   }
        # }
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
#' @importFrom rlang eval_tidy is_quosure prim_name

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
              clusterSpecs <- paste0("\n    origin : ", paste(top, left, collapse = ", ", sep = "|"), "  (row|col)",
                                     ifelse(!is.null(clusters$group), paste0("\n    groups : ", clusters$group), ""),
                                     ifelse(!is.null(clusters$id), paste0("\n    id     : ", clusters$id), ""))
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
              } else if(is_quosure(variables[[x]]$row)){
                prim_name(eval_tidy(variables[[x]]$row))
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
              } else if(is_quosure(variables[[x]]$col)){
                prim_name(eval_tidy(variables[[x]]$col))
              } else {
                temp <- unique(variables[[x]]$col)
                # make a short sequence of 'theRows'
                if(is.numeric(temp)){
                  dists <- temp - c(temp[1]-1, temp)[-(length(temp)+1)]
                } else {
                  dists <- 0
                }
                if(all(dists == 1) & length(temp) > 1){
                  paste0(min(temp), ":", max(temp))
                } else {
                  temp
                }
              }
            })
            nCols <- sapply(seq_along(theCols), function(x){
              ifelse(test = is.null(theCols[[x]]), yes = 0, no = nchar(paste0(theCols[[x]], collapse = ", ")))
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
            if(all(theRels == "F")){
              included <- c(included, FALSE)
            } else {
              included <- c(included, TRUE)
            }

            # whether variables are distinct
            theDist <- sapply(seq_along(variables), function(x){
              str_sub(as.character(variables[[x]]$dist), 1, 1)
            })
            if(all(theDist == "F")){
              included <- c(included, FALSE)
            } else {
              included <- c(included, TRUE)
            }

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
                if(included[7]){
                  head7 <- paste0("rel   ")
                  line7 <- paste0(c(rep("-", 5), " "), collapse = "")
                } else {
                  head7 <- line7 <- ""
                }
                if(included[8]){
                  head8 <-paste0("dist")
                  line8 <- paste0(c(rep("-", 6), " "), collapse = "")
                } else {
                  head8 <- line8 <- ""
                }

                cat(paste0(head1, head2, head3, head4, head5, head6, head7, head8), "\n")
                cat(" ", paste0(line1, line2, line3, line4, line5, line6, line7, line8), "\n")

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
                if(included[7]){
                  var7 <- paste0(theRels[[i-1]], "     ")
                } else {
                  var7 <- ""
                }
                if(included[8]){
                  var8 <- paste0(theDist[[i-1]], "  ")
                } else {
                  var8 <- ""
                }

                cat(paste0(var1, var2, var3, var4, var5, var6, var7, var8, "\n"))

              }


            }

          })
