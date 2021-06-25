#' Set where the clusters are
#'
#' There is hardly any limit to how data can be arranged in a spreadsheet, apart
#' from the apparent organisation into a lattice of cells. However, it is often
#' the case that data are gathered into topologically coherent chunks. Those
#' chunks are what is called 'cluster' in \code{tabshiftr}.
#' @param schema [\code{schema(1)}]\cr In case this information is added to an
#'   already existing schema, provide that schema here (overwrites previous
#'   information).
#' @param id [\code{character(1)}]\cr When data are clustered, it is typically
#'   the case that the data are segregated according to a categorical variables
#'   of interest. In such cases, this variable needs to be registered as cluster
#'   ID.
#' @param group [\code{character(1)}]\cr When clusters themselves are
#'   clustered, they are typically nested into another categorical variable,
#'   which needs to be registered as group ID.
#' @param left [\code{integerish(.)}]\cr The horizontal cell value of the
#'   top-left cell of each cluster. This can also be a vector of values in case
#'   there are several clusters.
#' @param top [\code{integerish(.)}]\cr The vertical cell values of the top-left
#'   cell of each cluster. This can also be a vector of values in case there are
#'   several clusters.
#' @param width [\code{integerish(.)}]\cr The width of each cluster in cells.
#'   This can also be a vector of values in case there are several clusters.
#' @param height [\code{integerish(.)}]\cr The height of each cluster in cells.
#'   This can also be a vector of values in case there are several clusters.
#' @param member [\code{integerish(.)}]\cr For each cluster, specify here to
#'   which group it belongs. Clusters are enumerated from left to right and
#'   from top to bottom.
#' @details Please also take a look at the currently suggested strategy to set
#'   up a \link[=schema]{schema description}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # please check the vignette for examples
#' @family functions to describe table arrangement
#' @importFrom checkmate assertClass assertCharacter assertIntegerish
#' @export

setCluster <- function(schema = NULL, id = NULL, group = NULL, member = NULL,
                       left = NULL, top = NULL, width = NULL, height = NULL){

  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = id, len = 1, any.missing = FALSE)
  assertCharacter(x = group, len = 1, any.missing = FALSE, null.ok = TRUE)
  colInt <- testIntegerish(x = left, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = left, len = 2)
  assert(colInt, colList)
  rowInt <- testIntegerish(x = top, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = top, len = 2)
  assert(rowInt, rowList)
  assertIntegerish(x = width, null.ok = TRUE)
  assertIntegerish(x = height, null.ok = TRUE)
  assertIntegerish(x = member, null.ok = TRUE)

  if(is.null(schema)){
    schema <- schema_default
  }

  if(!is.null(left)){
    schema@clusters$col <- left
  } else {
    schema@clusters$col <- 1
  }

  if(!is.null(top)){
    schema@clusters$row <- top
  } else {
    schema@clusters$row <- 1
  }

  if(!is.null(width)){
    schema@clusters$width <- width
  }

  if(!is.null(height)){
    schema@clusters$height <- height
  }

  if(!is.null(id)){
    schema@clusters$id <- id
  }

  if(!is.null(group)){
    schema@clusters$group <- group
  }

  if(!is.null(member)){
    schema@clusters$member <- member
  }

  # need a test that ensures header is included in the cluster


  return(schema)
}