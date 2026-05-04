#' Set Groups
#'
#' This function allows to set groups for rows, columns or clusters that shall
#' be summarised.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{list(3)}]\cr the output of \code{\link{.sum}} indicating
#'   the rows and a function according to which those rows should be summarised.
#' @param columns [\code{list(3)}]\cr the output of \code{\link{.sum}}
#'   indicating the columns and a function according to which those columns
#'   should be summarised.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @export

setGroups <- function(schema = NULL, rows = NULL, columns = NULL){

  # assertions ----
  if(!is.null(schema) && !inherits(schema, "schema"))
    stop("setGroups(): 'schema' must be a schema object (created by a previous setter call) or NULL.")

  # logical constraints ----
  # .sum() returns list(group = list(by = ..., groups = ..., fill = ...))
  if(!is.null(rows)) {
    if(!is.list(rows) || length(rows) != 1 || !all(c("by", "groups") %in% names(rows[[1]])))
      stop("setGroups(): 'rows' must be the output of .sum(...), not a plain list or vector. ",
           "Use rows = .sum(c(row1, row2), fill = \"down\") to define grouped rows.")
  }
  if(!is.null(columns)) {
    if(!is.list(columns) || length(columns) != 1 || !all(c("by", "groups") %in% names(columns[[1]])))
      stop("setGroups(): 'columns' must be the output of .sum(...), not a plain list or vector. ",
           "Use columns = .sum(c(col1, col2), fill = \"down\") to define grouped columns.")
  }

  # update schema ----
  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(rows)){
    schema@groups$rows <- c(schema@groups$rows, rows)
  }

  if(!is.null(columns)){
    schema@groups$cols <- c(schema@groups$cols, columns)
  }

  return(schema)

}
