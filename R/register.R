#' Register table properties (function is outdated)
#'
#' This functions registers properties of tables in preparation of
#' rectangularisation.
#' @param input [\code{data.frame(1)}]\cr table for which to register
#'   properties.
#' @param what [\code{character(1)}]\cr properties that should be registered as
#'   deviating from the default. Possible values are \code{schema},
#'   \code{format}, \code{clusters}, \code{commodities}, \code{adminUnits},
#'   \code{years}.
#' @param export [\code{logical(1)}]\cr whether or not to export the meta data
#'   as \code{XML}.
#' @param ... [\code{various}]\cr property specific arguments; see Details.
#' @details Property-specific arguments are:\itemize{ \item \code{schema}:
#'   \itemize{\item \code{name = ...}} A schema is a list of table
#'   properties. When calling it, \code{register} sets all of the properties
#'   specified in the schema. \item \code{format:} \itemize{ \item \code{type
#'   = "long" | "wide"} - if each variable is in one column, the table is long;
#'   if some variables are spread over several columns, the table is wide.}
#'   \item \code{clusters:} \itemize{\item \code{row} - the row number(s) of the
#'   top-left corner of each cluster. \item \code{col} - the column number(s) of
#'   the top-left corner of each cluster. \item \code{width} - the width of
#'   clusters (in number of cells) can either have one value (when all clusters
#'   have the same width) or as many values as there are clusters. \item
#'   \code{height} - the height of clusters (in number of cells) can either have
#'   one value (when all clusters have the same height) or as many values as
#'   there are clusters.} This could for instance be \code{'row = c(2, 52), col
#'   = 3, width = 4'}, if there are 2 rows of clusters that are 4 wide, starting
#'   at cell 2,3 and 52,3 \item \code{commodities}: }
#' @importFrom rlang exprs
#' @importFrom checkmate assertDataFrame assertCharacter assertChoice assertList
#'   assertNames
#' @export

register <- function(input, what = NULL, export = FALSE, ...){

  warning("this function is outdated!")

  # set internal objects
  args <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertDataFrame(x = input)
  assertCharacter(x = what, any.missing = FALSE)
  assertChoice(x = what, choices = c("schema", "format", "clusters", "commodities", "adminUnits", "years"))
  assertList(x = args)

  # check whether there is already a metadata object
  if(exists(x = "dmt_current", envir = baseenv())){
    default <- get(x = "dmt_current", envir = baseenv())
  } else{
    assign(x = "dmt_default", value = reg_default, envir = baseenv())
    assign(x = "dmt_current", value = reg_default, envir = baseenv())
  }

  # modify 'meta' according to what has been specified
  if(what == "schema"){
    assertNames(x = names(args), must.include = "name")
    meta <- get(x = eval(parse(text = args$name)))

    # check whether the schema can be used on 'input'

  } else {
    if(what == "format"){

    } else if(what == "clusters"){

    } else if(what == "commodities"){

    } else if(what == "adminUnits"){

    } else if(what == "years"){

    }
  }

  # assign the modified meta-object into the base-environment
  assign(x = "dmt_current", value = meta, envir = baseenv())

  if(export){
    # write 'meta' as xml into the working directory

  }

  return(input)

}