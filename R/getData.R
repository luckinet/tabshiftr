#' Extract summarised data
#'
#' This function extracts data from a table that are summarised by applying a
#' schema description to it.
#' @param schema [\code{character(1)}]\cr the (validated) schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @return a table where columns and rows are summarised
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
#' @importFrom checkmate assertTRUE
#' @importFrom dplyr row_number group_by summarise na_if across select mutate
#'   if_else
#' @importFrom tibble as_tibble
#' @importFrom tidyselect everything
#' @importFrom rlang eval_tidy
#' @export

getData <- function(schema = NULL, input = NULL){

  assertTRUE(x = schema@validated)

  filter <- schema@filter
  groups <- schema@groups

  out <- input

  if(!is.null(groups$rows)){

    isNumeric <- suppressWarnings(out %>%
      mutate(across(everything(), ~if_else(!is.na(as.numeric(.x)), TRUE, FALSE)))) %>%
      mutate(ind = as.double(row_number()))

    out <- out %>%
      mutate(ind = as.double(row_number()))
    outChar <- outNum <- out

    for(i in seq_along(groups$rows)){

      temp <- groups$rows[[i]]
      targetRows <- eval_tidy(temp$groups[[1]])

      outChar <- outChar %>%
        mutate(ind = if_else(ind %in% targetRows, min(targetRows), ind)) %>%
        group_by(ind) %>%
        summarise(across(everything(), eval_tidy(temp$by$char))) %>%
        mutate(across(everything(), ~na_if(x = ., y = "")))

      outNum <- suppressWarnings(outNum %>%
        mutate(ind = if_else(ind %in% targetRows, min(targetRows), ind)) %>%
        group_by(ind) %>%
        mutate(across(everything(), as.numeric)) %>%
        summarise(across(everything(), eval_tidy(temp$by$num))))

      isNumeric <- isNumeric %>%
        mutate(ind = if_else(ind %in% targetRows, min(targetRows), ind)) %>%
        group_by(ind) %>%
        summarise(across(everything(), ~if_else(any(.x), TRUE, FALSE)))

    }

    dims <- dim(isNumeric); dims[2] <- dims[2]-1
    isNumeric <- isNumeric %>%
      select(-ind) %>%
      unlist()
    outChar <- outChar %>%
      select(-ind) %>%
      unlist()
    outNum <- outNum %>%
      select(-ind) %>%
      unlist()

    outChar[isNumeric] <- as.character(outNum[isNumeric])

    out <- as_tibble(matrix(data = outChar, nrow = dims[1], ncol = dims[2]), .name_repair = "minimal")
    colnames(out) <- paste0("X", 1:(dims[2]))

  }

  # if(!is.null(groups$cols)){
  #
  #   for(i in seq_along(groups$cols)){
  #
  #     temp <- groups$cols[[1]]
  #
  #   }
  #
  # }


  return(out)
}
