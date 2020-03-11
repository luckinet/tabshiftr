#' Make a schema description
#'
#' @param schema [\code{list(2)}]\cr the list of schema information.
#' @importFrom checkmate assertList
#' @importFrom methods new
#' @export

makeSchema <- function(schema = NULL){

  clusters <- schema$clusters
  if(!any(names(clusters) %in% "top")){
    clusters <- c(clusters, list(dist = NULL))
  }
  if(!any(names(clusters) %in% "left")){
    clusters <- c(clusters, list(left = NULL))
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
  if(!any(names(meta) %in% "types")){
    meta <- c(meta, list(types = NULL))
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