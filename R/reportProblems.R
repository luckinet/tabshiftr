#' Catch and report problems in a schema description
#'
#' This function checks the current setup of a schema and reports problems that
#' will lead to an error of \code{reorganise} if not fixed.
#' @param schema [\code{character(1)}]\cr the schema description to check.

reportProblems <- function(schema = NULL){

  clusters <- schema@clusters
  format <- schema@format
  groups <- schema@groups
  filter <- schema@filter
  variables <- schema@variables

  # obvious errors
  # 1. a wide observed variable is defined, but no matching wide id variable
  # in setObsVar:
  # - ensure that 'value' is actually a value of the provided column in 'key'
  # in setIDVar:
  # - ensure that if dist = TRUE, values are absolute and the defined fields contain valid values
  # - ensure that relative values are still within the cluster
  # - all the below
  #   # if(!is.null(columns)){
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


}