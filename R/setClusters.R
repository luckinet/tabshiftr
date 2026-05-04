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
#' @importFrom checkmate testIntegerish testList
#' @export

setCluster <- function(schema = NULL, id = NULL, group = NULL, member = NULL,
                       left = NULL, top = NULL, width = NULL, height = NULL){

  # assertions ----
  if(!is.null(schema) && !inherits(schema, "schema"))
    stop("setCluster(): 'schema' must be a schema object (created by a previous setter call) or NULL.")
  if(is.null(id) || !is.character(id) || length(id) != 1 || nchar(id) == 0)
    stop("setCluster(): 'id' must be a non-empty character string naming the cluster variable, ",
         "e.g. id = \"territories\". Every cluster must have an identifying variable name.")
  if(!is.null(group) && (!is.character(group) || length(group) != 1 || nchar(group) == 0))
    stop("setCluster(): 'group' must be a non-empty character string naming the grouping variable, ",
         "e.g. group = \"region\".")
  colInt <- testIntegerish(x = left, lower = 1, min.len = 1, null.ok = TRUE)
  colList <- testList(x = left, len = 1)
  if(!is.null(left) && !colInt && !colList)
    stop("setCluster(): 'left' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(left), collapse = "/"), ".")
  if(colList) {
    if(!identical(names(left), "find"))
      stop("setCluster(): a list passed to 'left' must be a .find() call (named 'find').")
  }
  rowInt <- testIntegerish(x = top, lower = 1, min.len = 1, null.ok = TRUE)
  rowList <- testList(x = top, len = 1)
  if(!is.null(top) && !rowInt && !rowList)
    stop("setCluster(): 'top' must be a positive integer vector (>= 1) or a .find() call. Got: ",
         paste(class(top), collapse = "/"), ".")
  if(rowList) {
    if(!identical(names(top), "find"))
      stop("setCluster(): a list passed to 'top' must be a .find() call (named 'find').")
  }
  if(!is.null(width) && !testIntegerish(width, lower = 1))
    stop("setCluster(): 'width' must be a positive integer (>= 1). Got: ", paste(width, collapse = ", "), ".")
  if(!is.null(height) && !testIntegerish(height, lower = 1))
    stop("setCluster(): 'height' must be a positive integer (>= 1). Got: ", paste(height, collapse = ", "), ".")
  if(!is.null(member) && !testIntegerish(member, lower = 1))
    stop("setCluster(): 'member' must be a positive integer vector (>= 1). Got: ",
         paste(class(member), collapse = "/"), ".")

  # logical constraints ----
  if(!is.null(schema) && !is.null(schema@clusters$id))
    warning("setCluster(): the schema already has a cluster definition. Calling setCluster() again will overwrite it. ",
            "If you meant to define multiple clusters, provide all origins in a single setCluster() call (e.g., top = c(3, 9), left = c(1, 1)).")
  if(!is.null(group) && is.null(member))
    stop("setCluster(): 'group' is set to '", group, "' but 'member' is not provided. ",
         "For nested clusters, 'member' must be an integer vector assigning each cluster to a group.")
  if(is.null(group) && !is.null(member))
    stop("setCluster(): 'member' is set but 'group' is not. ",
         "'member' only makes sense with 'group' (nested clusters). Did you forget group = \"...\"?")
  nOrigins <- max(length(top), length(left), na.rm = TRUE)
  if(!is.null(member) && nOrigins > 0 && length(member) != nOrigins)
    stop("setCluster(): 'member' has length ", length(member), " but there are ", nOrigins,
         " clusters (from 'top'/'left'). 'member' must have one entry per cluster.")

  # update schema ----
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

  return(schema)
}
