#' Extract clusters
#'
#' This function extracts clusters of data from a table by applying a schema
#' description to it.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @return list of the length of number of clusters with clusters cut out from
#'   the original input
#' @examples
#' input <- tabs2shift$clusters_nested
#' schema <- setCluster(id = "sublevel",
#'                      group = "territories", member = c(1, 1, 2),
#'                      left = 1, top = c(3, 8, 15)) %>%
#'   setIDVar(name = "territories", columns = 1, rows = c(2, 14)) %>%
#'   setIDVar(name = "sublevel", columns = 1, rows = c(3, 8, 15)) %>%
#'   setIDVar(name = "year", columns = 7) %>%
#'   setIDVar(name = "commodities", columns = 2) %>%
#'   setObsVar(name = "harvested", columns = 5) %>%
#'   setObsVar(name = "production", columns = 6)
#'
#' validateSchema(schema = schema, input = input) %>%
#'    getData(input = input)
#' @importFrom purrr map map_dfr
#' @importFrom tibble as_tibble
#' @importFrom dplyr n summarise
#' @importFrom stats na.omit
#' @export

getData <- function(schema = NULL, input = NULL){

  clusters <- schema@clusters
  groups <- schema@groups
  nClusters <- max(lengths(clusters))

  if(any(lengths(groups) > 0)){

    tempGroups <- 1:dim(input)[1]
    for(i in seq_along(groups$rows$ind)){
      temp <- eval_tidy(groups$rows$ind[[i]])
      tempGroups[temp] <- max(tempGroups) + 1
    }

    tempInput <- input %>%
      mutate(grps = tempGroups)
    tempGroups <- tempGroups %>%
      as_tibble() %>%
      group_by(value) %>%
      summarise(n = n())

    out <- map_dfr(seq_along(tempGroups$value), function(ix){

      temp <- tempInput %>%
        filter(grps == tempGroups$value[ix]) %>%
        select(-grps)

      if(tempGroups$n[ix] == 1){
        return(temp)
      } else {

        nums <- suppressWarnings(temp %>% mutate(across(everything(), as.numeric)))
        nums <- nums %>% summarise(across(everything(), sum))
        # targetCols <- get columns where all rows are NA

        chars <- temp %>% summarise(across(everything(), function(x){  paste0(na.omit(x), collapse = " ")}))

        bind_cols(chars[targetCols], nums[!targetCols])
      }

    })

  # } else if(!is.null(clusters$id)){
  #   out <- map(.x = 1:nClusters, .f = function(ix){
  #
  #     input[
  #       clusters$row[ix]:(clusters$row[ix]+clusters$height[ix] - 1),
  #       clusters$col[ix]:(clusters$col[ix]+clusters$width[ix] - 1)
  #     ]
  #
  #   })
  } else {
    out <- input
  }

  return(out)
}