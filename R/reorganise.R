#' Reorganise a table
#'
#' This function takes a disorganised messy table and rearranges columns and
#' rows into a tidy table based on a schema description.
#' @param input [\code{data.frame(1)}]\cr table to reorganise.
#' @param schema [\code{symbol(1)}]\cr the schema description of \code{input}.
#' @return A (tidy) table which is the result of reorganising \code{input} based
#'   on \code{schema}.
#' @examples
#' # a rather disorganised table with messy clusters and a distinct variable
#' (input <- tabs2shift$clusters_messy)
#'
#' # put together schema description by ...
#' # ... identifying cluster positions
#' schema <- setCluster(id = "territories", left = c(1, 1, 4), top = c(1, 8, 8))
#'
#' # ... specifying the cluster ID as id variable (obligatory)
#' schema <- schema %>%
#'     setIDVar(name = "territories", columns = c(1, 1, 4), rows = c(2, 9, 9))
#'
#' # ... specifying the distinct variable (explicit position)
#' schema <- schema %>%
#'     setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE)
#'
#' # ... specifying a tidy variable (by giving the column values)
#' schema <- schema %>%
#'     setIDVar(name = "commodities", columns = c(1, 1, 4))
#'
#' # ... identifying the (tidy) observed variables
#' schema <- schema %>%
#'     setObsVar(name = "harvested", columns = c(2, 2, 5)) %>%
#'     setObsVar(name = "production", columns = c(3, 3, 6))
#'
#' # get the tidy output
#' reorganise(input, schema)
#'
#' @importFrom checkmate assertDataFrame assertIntegerish
#' @importFrom dplyr filter_all any_vars bind_rows slice group_by ungroup select
#'   mutate arrange bind_cols rename arrange_at filter mutate_if left_join
#'   mutate_all
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom magrittr %>%
#' @importFrom rlang set_names
#' @importFrom purrr reduce map map_int
#' @export

reorganise <- function(input = NULL, schema = NULL){

  # library(tidyverse); library(rlang); library(checkmate)

  # check validity of arguments
  assertDataFrame(x = input)

  input <- input %>%
    mutate_all(as.character)

  # 1. add missing information in schema ----
  schema <- validateSchema(input = input, schema = schema)

  # 2. select data from cluster specs ----
  clusters <- getData(input = input, schema = schema)

  # if a cluster id has been specified, extract the variable values
  clusterVar <- getClusterVar(input = input, schema = schema)

  # if a group id has been specified, extract the variable values
  groupVar <- getGroupVar(input = input, schema = schema)

  # select the id variables
  idVars <- getIDVars(input = input, schema = schema)

  # select the observed variables
  obsVars <- getObsVars(input = input, schema = schema)

  nClusters <- length(idVars)

  theValues <- list()
  for(i in 1:nClusters){

    # match all of the readily available variables
    # ids = idVars[[i]]; obs = obsVars[[i]]; clust = clusterVar[i]; grp = groupVar[i]
    tidyVars <- .tidyVars(ids = idVars[[i]], obs = obsVars[[i]],
                          clust = clusterVar[i], grp = groupVar[i])

    # put together the table
    theTable <- bind_cols(tidyVars, .name_repair = "minimal")
    names(theTable) <- names(tidyVars)

    # append the data to the overall output list
    theValues <- c(theValues, list(theTable))

  }

  clustNames <- map(.x = seq_along(theValues), .f = function(ix){
    names(theValues[[ix]])
  })

  differentNames <- isFALSE(reduce(.x = clustNames, .f = function(x,y) if (identical(x,y)) x else FALSE))

  if(differentNames){
    out <- suppressMessages(reduce(theValues, left_join))
  } else {
    out <- bind_rows(theValues)
  }

  out <- out %>%
    unnest(cols = c(names(schema@variables))) %>%
    .updateFormat(schema = schema) %>%
    select(names(schema@variables))

  return(out)
}
