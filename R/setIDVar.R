#' Set an identifying variable
#'
#' Identifying variables are those variables that describe the (qualitative)
#' properties that make each observation (as described by the
#' \code{\link[=setObsVar]{observed variables}}) unique.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param name [\code{character(1)}]\cr Name of the new identifying variable.
#' @param value [\code{character(1)}]\cr In case the variable is an implicit
#'   variable (i.e., which is not in the origin table), specify it here.
#' @param columns [\code{integerish(.)}]\cr The column(s) in which the
#'   \emph{values} of the new variable are recorded.
#' @param rows [\code{integerish(.)}]\cr In case the variable is in several
#'   columns, specify here additionally the row in which the names are recorded.
#' @param split [\code{character(1)}]\cr In case the variable is part of a
#'   compound value, this should be a regular expression that splits the
#'   respective value off of that compound value.
#' @param merge [\code{character(1)}]\cr In case a variable is made up of
#'   several columns, this should be the character string that would connect the
#'   two columns (e.g., an empty space \code{" "}).
#' @param relative [\code{logical(1)}]\cr whether or not the values provided in
#'   \code{columns} and \code{rows} are relative to the cluster position(s) or
#'   whether they refer to the overall table.
#' @param distinct [\code{logical(1)}]\cr Whether or not the variable is
#'   distinct from a cluster. This is the case when the variable is not
#'   systematically available for all clusters and thus needs to be registered
#'   separately from the clusters.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertCharacter assertLogical
#'   testIntegerish testList
#' @export

setIDVar <- function(schema = NULL, name = NULL, value = NULL, columns = NULL,
                     rows = NULL, split = NULL, merge = NULL, relative = FALSE,
                     distinct = FALSE){

  # assertions ----
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 2)
  assert(colInt, colList)
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows, len = 2)
  assert(rowInt, rowList)
  assertCharacter(x = value, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = split, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = merge, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = relative, any.missing = FALSE, len = 1)
  assertLogical(x = distinct, any.missing = FALSE, len = 1)

  if(is.null(schema)){
    schema <- schema_default
  }
  nClusters <- max(lengths(schema@clusters))
  if(nClusters == 0) nClusters <- 1
  prevIDcols <- unlist(lapply(seq_along(schema@variables), function(x){
    if(schema@variables[[x]]$typ == "id"){
      if(is.null(schema@variables[[x]]$row)){
        schema@variables[[x]]$col
      }
    }
  }))

  # error management ----
  # if(!is.null(columns)){
  #   # ensure that a row is set, in case the variable is contained in several columns
  #   if(nClusters == 1){
  #     if(length(columns) > 1){
  #       if(is.null(rows)){
  #         if(is.null(merge)){
  #           message("  -> the variable '", name, "' is wide (i.e., in several columns), but no row with the names, nor the merge option is set.")
  #         }
  #       }
  #     } else{
  #       if(!is.null(rows)){
  #         message("  -> 'row' is set for the variable '", name, "', even though it is not needed.")
  #       }
  #     }
  #   }
  #   # ensure that a split expression is set, in case the variable is contained in a column that already contains another variable
  #   if(!colQuo){
  #     if(any(columns %in% prevIDcols)){
  #       if(is.null(split)){
  #         message("  -> the variable '", name, "' is in a column (", paste(columns, collapse = ", "), ") that already contains another variable, but no split-expression is set.")
  #       }
  #     }
  #   }
  # } else{
  #   # if(!is.null(rows)){
  #   #   message("  -> 'rows' is set for the variable '", name, "', even though it is not needed.")
  #   # }
  # }

  # ensure that split results in a non-empty value
  # if(!is.null(split)){
  #   if(is.null(columns)){
  #     message("  -> the variable '", name, "' has a split-expression, but no column is set.")
  #   } else {
  #     # test that the split expression doesn't lead to an empty value
  #     # recently not yet defined to have the input table in an environment for those "in-situ" tests
  #   }
  # }

  # ensure that when not using 'value', either columns or rows is set
  # if(is.null(value)){
  #   if(is.null(columns) & is.null(rows)){
  #     message("  -> for the variable '", name, "' there is neither an explicit 'value' set, nor are there any column(s) (and rows).")
  #   }
  # }

  # in case the user thought that it's sufficient to specify a row
  # if(!is.null(rows)){
  #   if(is.null(columns)){
  #     message("  -> in case the variable '", name, "' is in several columns, set first those columns and then the row of the variable names.")
  #   } else{
  #     # test that the column/row combination (here the variable names should be) leads to non-empty character values
  #
  #   }
  # }

  # ensure that relative values are still within the cluster
  # if(nClusters != 0){
  #   if(relative){
  #     if(!is.null(schema@clusters$width)){
  #
  #     }
  #     if(!is.null(schema@clusters$height)){
  #
  #     }
  #   }
  # }

  # ensure that if dist = TRUE, values are absolute and the defined fields contain valid values




  # update schema ----
  temp <- list(type = "id",
               value = value,
               col = columns,
               row = rows,
               split = split,
               merge = merge,
               rel = relative,
               dist = distinct)
  schema@variables[[name]] <- temp

  return(schema)
}