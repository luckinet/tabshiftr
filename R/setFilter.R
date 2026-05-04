#' Set filters
#'
#' This function allows to specify additional rules to filter certain rows
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param rows [\code{integerish(.)}]\cr rows that are mentioned here are kept.
#' @param columns [\code{integerish(.)}]\cr columns that are mentioned here are
#'   kept.
#' @param invert [\code{logical(1)}]\cr whether or not to invert the specified
#'   columns or rows. When inverting row filters to select data rows (i.e.,
#'   specifying the rows to exclude rather than keep), the column-header row must
#'   be included explicitly in \code{rows}. This is because the header is
#'   reconstructed from column names at a later stage and is not present in the
#'   table when the schema is evaluated; it will not be excluded automatically.
#' @param clusters [\code{logical(1)}]\cr whether or not to filter cluster rows.
#' @param operator [\code{function(1)}]\cr \code{\link[base]{Logic}} operators
#'   by which the current filter should be combined with the directly preceeding
#'   filter; hence this argument is not used in case no other filter was defined
#'   before it.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' (input <- tabs2shift$messy_rows)
#'
#' # select rows where there is 'unit 2' in column 1 or 'year 2' in column 2
#' schema <-
#'   setFilter(rows = .find(pattern = "unit 2", col = 1)) %>%
#'   setFilter(rows = .find(pattern = "year 2", col = 2), operator = `|`) %>%
#'   setIDVar(name = "territories", columns = 1) %>%
#'   setIDVar(name = "year", columns = 2) %>%
#'   setIDVar(name = "commodities", columns = 3) %>%
#'   setObsVar(name = "harvested", columns = 5) %>%
#'   setObsVar(name = "production", columns = 6)
#'
#' reorganise(schema = schema, input = input)
#' @family functions to describe table arrangement
#' @importFrom checkmate testIntegerish testList
#' @export

setFilter <- function(schema = NULL, rows = NULL, columns = NULL, invert = FALSE,
                      clusters = TRUE, operator = NULL){

  # assertions ----
  if(!is.null(schema) && !inherits(schema, "schema"))
    stop("setFilter(): 'schema' must be a schema object (created by a previous setter call) or NULL.")
  rowInt <- testIntegerish(x = rows, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = rows, len = 1)
  if(!is.null(rows) && !rowInt && !rowList)
    stop("setFilter(): 'rows' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(rows), collapse = "/"), ".")
  if(rowList) {
    if(!identical(names(rows), "find"))
      stop("setFilter(): a list passed to 'rows' must be a .find() call (named 'find').")
  }
  colInt <- testIntegerish(x = columns, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = columns, len = 1)
  if(!is.null(columns) && !colInt && !colList)
    stop("setFilter(): 'columns' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(columns), collapse = "/"), ".")
  if(colList) {
    if(!identical(names(columns), "find"))
      stop("setFilter(): a list passed to 'columns' must be a .find() call (named 'find').")
  }
  if(!is.logical(invert) || anyNA(invert))
    stop("setFilter(): 'invert' must be TRUE or FALSE.")
  if(!is.logical(clusters) || anyNA(clusters))
    stop("setFilter(): 'clusters' must be TRUE or FALSE.")

  # logical constraints ----
  if(is.null(rows) && is.null(columns))
    message("setFilter(): called with both 'rows' and 'columns' as NULL. This call has no effect.")

  # update schema ----
  if(is.null(schema)){
    schema <- schema_default
  }

  if(is.null(operator)){
    operator <- `&`
  }

  if(!is.null(rows)){
    if(!is.list(rows)){
      rows <- list(position = rows, invert = invert)
    }
    if(!is.null(schema@filter$row)){
      rows <- c(operator = operator, rows)
    }
    schema@filter$row <- c(schema@filter$row, rows)
  }

  if(!is.null(columns)){
    if(!is.list(columns)){
      columns <- list(position = columns)
    }
    if(!is.null(schema@filter$row)){
      columns <- c(operator = operator, columns)
    }
    schema@filter$col <- c(schema@filter$col, columns)
  }

  schema@filter$clusters <- clusters

  return(schema)

}
