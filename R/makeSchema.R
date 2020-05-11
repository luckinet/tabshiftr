#' Make a schema description
#'
#' This function checks whether the schema description is formally correct.
#'
#' @section Setting up schema descriptions: The recommended strategy for setting
#'   up a schema description is the following recently. \enumerate{ \item
#'   Clarify which are the identifying variables and which are the measured
#'   variables and create a new entry for each of them in the schema. \item
#'   Determine whether there are clusters and find the origin (top left cell) of
#'   each cluster. Follow the next steps for each cluster... \item Determine
#'   which variable identifies clusters and provide that as cluster ID. \item
#'   Determine for each identifying variable the following: \itemize{ \item is
#'   the variable available at all? If not, provide the variable value for this
#'   cluster in `value`. \item all columns in which the variable \emph{names}
#'   sit. \item in case the variable is in several columns, determine
#'   additionally the row in which its \emph{names} sit. \item whether the
#'   variable is distinct from the main table. \item whether the variable must
#'   be split off of another column. } \item Determine for each measured
#'   variable the following: \itemize{ \item all columns in which the
#'   \emph{values} of the variable sit. \item the unit and conversion factor
#'
#'   \item in case the variable is not tidy, one of the three following cases
#'   should apply: \enumerate{ \item in case the variable is nested in a wide
#'   identifying variable, determine in addition to the columns in which the
#'   values sit also the rows in which the \emph{variable name} sits. \item in
#'   case the names of the variable are given as a value of an identifying
#'   variable, give the column name as \code{key}, together with the respective
#'   name of the measured variable in \code{values}. \item in case the name of
#'   the variable is the ID of clusters, specify \code{key = "cluster"} and in
#'   \code{values} the cluster number the variable refers to. } } } See below
#'   for a more detailed description of the fields used in schemas or read the
#'   vignette.
#'
#' @section Fields of a schema: The following section contains a list of all the
#'   fields recently evaluated in a schema. The information is split up into the
#'   four sub-sections \emph{clusters}, \emph{header}, \emph{meta} and
#'   \emph{variables}.
#'
#'   There is hardly any limit to how data can be arranged in a spreadsheet,
#'   apart from the apparent organisation into a lattice of cells. However, it
#'   is often the case that data are gathered into topologically coherent
#'   chunks. Those chunks are what is considered 'cluster' in arealDB. Clusters
#'   are described by the properties: \itemize{ \item \code{row}
#'   [\code{integerish(1)}]:\cr The vertical cell values of the top-left cell of
#'   each cluster. This can also be a vector of values in case there are several
#'   clusters. \item \code{col} [\code{integerish(1)}]:\cr The horizontal cell
#'   value of the top-left cell of each cluster. This can also be a vector of
#'   values in case there are several clusters. \item \code{width}
#'   [\code{integerish(1)}]:\cr The width of each cluster in cells. This can
#'   also be a vector of values in case there are several clusters \item
#'   \code{height} [\code{integerish(1)}]:\cr The height of each cluster in
#'   cells. This can also be a vector of values in case there are several
#'   clusters \item \code{id} [\code{character(1)}]:\cr When data are clustered,
#'   it is often the case that the data are segregated according to one of the
#'   variables of interest. In such cases, this variable needs to be registered
#'   as cluster ID.}
#'
#'   The slot \code{header} describes in which rows the header informations are
#'   stored and how they should be treated. It contains the properties \itemize{
#'   \item \code{row} [\code{integerish(.)}]\cr The rows in which the header
#'   information are stored. \item \code{rel} [\code{logical(1)}]\cr  Whether or
#'   not the values in \code{row} are relative to the cluster positions or
#'   whether they refer to the overall table. \item \code{merge}
#'   [\code{logical(1)}]\cr When there is more than one row, this determines
#'   whether or not those rows should be merged. }
#'
#'   The slot \code{meta} describes information concerning the values in a
#'   spreadsheet. It contains the properties \itemize{ \item \code{del}
#'   [\code{character(.)}]\cr The symbol(s) that are used as delimiter in the
#'   table to reorganise. \item \code{dec} [\code{character(.)}]\cr The
#'   symbol(s) that are used as decimal symbol in the table to reorganise. \item
#'   \code{na} [\code{character(.)}]\cr The symbol(s) that are used as "not
#'   available" values in the table to reorganise.}
#'
#'   Each element in the slot \code{variables} is a list that describes one of
#'   the variables that shall be comprised in the final database. Variables are
#'   either so-called \emph{identifying variables}, which indicate observation
#'   units, or \emph{measured variables}, which carry the observed values.
#'   Identifying variables are described by the properties: \itemize{ \item
#'   \code{type} [\code{character(1)}]:\cr The value \code{"id"} signals that
#'   this is an identifying variable. \item \code{col}
#'   [\code{integerish(1)}]:\cr The column(s) in which the variable values are
#'   recorded. \item \code{row} [\code{integerish(1)}]:\cr The row(s) in which
#'   the variable values are recorded. \item \code{split}
#'   [\code{character(1)}]:\cr A regular expression that matches the part of
#'   values that are supposed to be part of the variable, when the cells contain
#'   more than one value (for example separated by a ","). \item \code{dist}
#'   [\code{character(1)}]:\cr Whether or not the variable is distinct from a
#'   cluster. This is the case when the variable is not systematically available
#'   for all clusters and thus needs to be registered separately from the
#'   clusters. \item \code{rel} [\code{logical(1)}]:\cr Whether or not the
#'   values in \code{row} and \code{col} are relative to the cluster positions
#'   or whether they refer to the overall table. }
#'
#'   Measured variables are described by the properties: \itemize{ \item
#'   \code{type} [\code{character(1)}]:\cr the value \code{"measured"} signals
#'   that this is a measured variable. \item \code{unit} [\code{character(1)}]:\cr
#'   The unit in which the values shall be recorded in the database. \item
#'   \code{factor} [\code{character(1)}]:\cr A factor to transform the values to
#'   \code{unit}. For instance, if values are recorded in acres, but shall be
#'   contained in the database in hectare, the factor would be 0.40468. \item
#'   \code{row} [\code{integerish(1)}]:\cr The row(s) in which the variable
#'   values are recorded. \item \code{col} [\code{integerish(1)}]:\cr The
#'   column(s) in which the variable values are recorded. \item \code{rel}
#'   [\code{logical(1)}]:\cr Whether or not the values in \code{row} and
#'   \code{col} are relative to the cluster positions or whether they refer to
#'   the overall table. \item \code{key} [\code{character(1)}]:\cr If the
#'   variable is recorded, together with other variables, so that the variable
#'   names are listed in one column and the respective values are listed in
#'   another column, give here the name of the column that contains the variable
#'   names. \item \code{value} [\code{character(1)}]:\cr If the variable is
#'   recorded, together with other variables, so that the variable names are
#'   listed in one column and the respective values are listed in another
#'   column, give here the level in the names column that stands for the values
#'   of this variable. \item \code{dist} [\code{character(1)}]:\cr Whether or
#'   not the variable is distinct from a cluster. This is the case when the
#'   variable is not systematically available for all clusters and thus needs to
#'   be registered separately from the clusters. }
#' @param schema [\code{list(.)}]\cr the list of schema information. This can
#'   contain the lists \code{clusters}, \code{meta}, \code{header} and
#'   \code{variables}.
#' @return An object of class \code{\link{schema}}.
#' @examples
#' # define outline of the cluster(s)
#' theClusters <- list(row = c(1, 8, 8),
#'                     col = c(1, 1, 4),
#'                     width = 3,
#'                     height = 6,
#'                     id = "territories")
#'
#' # identify the row(s) where the header is
#' theHeader <- list(row = 1, rel = TRUE)
#'
#' # document identifying variables
#' idVars <- list(
#'   territories =
#'     list(type = "id", row = 1, col = 1, rel = TRUE),
#'   year =
#'     list(type = "id", row = c(3:6), col = 4, dist = TRUE),
#'   commodities =
#'     list(type = "id", col = 1, rel = TRUE))
#'
#' # document measured variables
#' measuredVars <- list(
#'   harvested =
#'     list(type = "measured", unit = "ha", factor = 1,
#'          col = 2, rel = TRUE),
#'   production =
#'     list(type = "measured", unit = "t", factor = 1,
#'          col = 3, rel = TRUE))
#'
#' # make the schema
#' mySchema <- list(clusters = theClusters,
#'                  header = theHeader,
#'                  variables = c(idVars, measuredVars))
#'
#' makeSchema(schema = mySchema)
#' @importFrom checkmate assertNames
#' @importFrom methods new
#' @export

makeSchema <- function(schema = NULL){

  clusters <- schema$clusters
  if(!any(names(clusters) %in% "row")){
    clusters <- c(clusters, list(row = NULL))
  }
  if(!any(names(clusters) %in% "col")){
    clusters <- c(clusters, list(col = NULL))
  }
  if(!any(names(clusters) %in% "width")){
    clusters <- c(clusters, list(width = NULL))
  }
  if(!any(names(clusters) %in% "height")){
    clusters <- c(clusters, list(height = NULL))
  }
  if(!any(names(clusters) %in% "id")){
    clusters <- c(clusters, list(id = NULL))
  }

  header <- schema$header
  if(!any(names(header) %in% "row")){
    header <- c(header, list(row = NULL))
  }
  if(!any(names(header) %in% "rel")){
    header <- c(header, list(rel = FALSE))
  }
  if(!any(names(header) %in% "merge")){
    header <- c(header, list(merge = FALSE))
  }

  meta <- schema$meta
  if(!any(names(meta) %in% "del")){
    meta <- c(meta, list(del = NULL))
  }
  if(!any(names(meta) %in% "dec")){
    meta <- c(meta, list(dec = NULL))
  }
  if(!any(names(meta) %in% "na")){
    meta <- c(meta, list(na = NULL))
  }
  # if(!any(names(meta) %in% "types")){
  #   meta <- c(meta, list(types = NULL))
  # }

  variables <- schema$variables
  for(i in seq_along(variables)){
    varProp <- variables[[i]]
    varName <- names(variables)[i]
    assertNames(x = names(varProp), must.include = "type")

    # set default values, if they haven't been set
    if(varProp$type == "id"){
      if(!any(names(varProp) %in% "value")){
        varProp <- c(varProp, list(value = NULL))
      }
      if(!any(names(varProp) %in% "split")){
        varProp <- c(varProp, list(split = NULL))
      }
      if(!any(names(varProp) %in% "row")){
        varProp <- c(varProp, list(row = NULL))
      }
      if(!any(names(varProp) %in% "col")){
        varProp <- c(varProp, list(col = NULL))
      }
      if(!any(names(varProp) %in% "rel")){
        varProp <- c(varProp, list(rel = FALSE))
      }
      if(!any(names(varProp) %in% "dist")){
        varProp <- c(varProp, list(dist = FALSE))
      }

    } else if(varProp$type == "measured"){
      if(!any(names(varProp) %in% "unit")){
        varProp <- c(varProp, list(unit = NULL))
      }
      if(!any(names(varProp) %in% "factor")){
        varProp <- c(varProp, list(factor = NULL))
      }
      if(!any(names(varProp) %in% "row")){
        varProp <- c(varProp, list(row = NULL))
      }
      if(!any(names(varProp) %in% "col")){
        varProp <- c(varProp, list(col = NULL))
      }
      if(!any(names(varProp) %in% "rel")){
        varProp <- c(varProp, list(rel = FALSE))
      }
      if(!any(names(varProp) %in% "dist")){
        varProp <- c(varProp, list(dist = FALSE))
      }
      if(!any(names(varProp) %in% "key")){
        varProp <- c(varProp, list(key = NULL))
      }
      if(!any(names(varProp) %in% "value")){
        varProp <- c(varProp, list(value = NULL))
      }
    }
    variables[[i]] <- varProp
  }

  out <- new(Class = "schema",
             clusters = clusters,
             header = header,
             meta = meta,
             variables = variables)

  return(out)

}