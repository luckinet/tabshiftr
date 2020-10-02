#' Check a schema description
#'
#' This function checks whether the schema description is formally correct.
#' @param schema [\code{list(.)}]\cr the list of schema information. This can
#'   contain the lists \code{clusters}, \code{format}, \code{header} and
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

  message("please don't use this function anymore, but instead look into setCluster, setHeader or setFormat")

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

  format <- schema$format
  if(!any(names(format) %in% "del")){
    format <- c(format, list(del = NULL))
  }
  if(!any(names(format) %in% "dec")){
    format <- c(format, list(dec = NULL))
  }
  if(!any(names(format) %in% "na")){
    format <- c(format, list(na = NULL))
  }

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
      if(!any(names(varProp) %in% "row")){
        varProp <- c(varProp, list(row = NULL))
      }
      if(!any(names(varProp) %in% "col")){
        varProp <- c(varProp, list(col = NULL))
      }
      if(!any(names(varProp) %in% "split")){
        varProp <- c(varProp, list(split = NULL))
      }
      if(!any(names(varProp) %in% "merge")){
        varProp <- c(varProp, list(merge = NULL))
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
      if(!any(names(varProp) %in% "key")){
        varProp <- c(varProp, list(key = NULL))
      }
      if(!any(names(varProp) %in% "value")){
        varProp <- c(varProp, list(value = NULL))
      }
      if(!any(names(varProp) %in% "rel")){
        varProp <- c(varProp, list(rel = FALSE))
      }
      if(!any(names(varProp) %in% "dist")){
        varProp <- c(varProp, list(dist = FALSE))
      }
    }
    variables[[i]] <- varProp
  }

  out <- new(Class = "schema",
             clusters = clusters,
             header = header,
             format = format,
             variables = variables)

  return(out)

}