#' Pre-process input table
#'
#' This function groups rows, splices the header into the table and fills
#' missing values where they should not exist.
#' @param schema [\code{character(1)}]\cr the validated schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @details
#' \code{validateInput} is called automatically by \code{\link{reorganise}} and
#' does not usually need to be called directly. It performs two pre-processing
#' steps on the input table before variable extraction begins:
#' \enumerate{
#'   \item If \code{setFormat(header = TRUE)} was used, the column names that
#'   were consumed by R when reading the file are spliced back into the table as
#'   row 1. This makes row numbers stable and consistent with the schema
#'   description.
#'   \item If \code{\link{setGroups}} was used, the specified groups of rows are
#'   summarised into single rows according to the aggregation functions provided
#'   to \code{\link{.sum}}. Character columns are collapsed with
#'   \code{paste0(na.omit(x), collapse = " ")} by default; numeric columns are
#'   summed. Missing values within a group can be filled before aggregation by
#'   passing a \code{fill} direction to \code{\link{.sum}}.
#' }
#' @return a table where grouped rows are summarised and, if applicable, the
#'   header row is spliced back in as row 1.
#' @examples
#' # validateInput is called implicitly by reorganise(); the example below shows
#' # its effect when setGroups is used to collapse pairs of rows before extraction.
#' (input <- tabs2shift$group_sum)
#'
#' schema <-
#'   setGroups(rows = .sum(c(3, 4))) |>
#'   setGroups(rows = .sum(c(6, 7))) |>
#'   setIDVar(name = "territories", columns = 1) |>
#'   setIDVar(name = "year", columns = 2) |>
#'   setIDVar(name = "commodities", columns = c(3:6), rows = 2) |>
#'   setObsVar(name = "harvested", columns = c(3, 4)) |>
#'   setObsVar(name = "production", columns = c(5, 6))
#'
#' # inspect the pre-processed table directly
#' schema_validated <- validateSchema(schema = schema, input = input)
#' validateInput(schema = schema_validated, input = input)
#' @importFrom checkmate assertTRUE
#' @importFrom dplyr row_number group_by summarise na_if across select mutate
#'   if_else arrange add_row slice
#' @importFrom tibble as_tibble as_tibble_row
#' @importFrom tidyselect everything where
#' @importFrom lubridate is.Date
#' @importFrom rlang eval_tidy
#' @export

validateInput <- function(schema = NULL, input = NULL){

  assertDataFrame(x = input)
  assertClass(x = schema, classes = "schema")
  assertTRUE(x = schema@validated)

  header <- schema@format$header
  groups <- schema@groups

  assertLogical(x = header, len = 1, any.missing = FALSE, null.ok = TRUE)

  # first splice the header into the table, if it hasn't been read without column names
  if(isTRUE(header)){

    input <- input %>%
      mutate(across(where(is.double) | where(is.integer) |  where(is.logical) | where(is.Date), as.character))
    non_char <- .getColTypes(input = input, collapse = FALSE) != "c"

    if(header != 1L){
      stop("! implement case where more than one rows need to be shifted !")
    } else {
      vec <- colnames(input)
      names(vec) <- paste0("X", seq_along(vec))
      vec <- as_tibble_row(vec)
      vec[, non_char] <- NA

      colnames(input) <- paste0("X", seq_along(vec))

      input <- bind_rows(vec, input)
    }

  }

  if(!is.null(groups$rows)){

    tempTab <- input |>
      mutate(rn = as.double(row_number()), .before = 1)

    isNumeric <- suppressWarnings(
      input %>%
        mutate(across(everything(), ~if_else(!is.na(as.numeric(.x)), TRUE, FALSE)))
    )

    for(i in seq_along(groups$rows)){

      temp <- groups$rows[[i]]

      charBy <- eval_tidy(temp$by$char)
      if(is.null(charBy)) charBy <- ~ paste0(na.omit(.x), collapse = " ")
      numBy <- eval_tidy(temp$by$num)
      if(is.null(numBy)) numBy <- ~ sum(.x, na.rm = TRUE)
      targetRows <- eval_tidy(temp$groups[[1]])

      tempRows <- input %>%
        slice(targetRows)
      typeRows <- isNumeric |>
        slice(targetRows)
      typeCols <- typeRows |>
        summarise(across(everything(), ~ any(.x)))

      # fill NA-values in grouped rows
      if(!is.null(temp$fill) & anyNA(tempRows)){
        for(j in seq_along(temp$fill)){
          tempRows <- .fill(x = tempRows, direction = temp$fill[j])
        }
      }

      if(any(typeRows)){

        charRows <- tempRows[!unlist(typeCols)] |>
          summarise(across(everything(), charBy)) |>
          mutate(across(where(is.character), ~na_if(x = ., y = "")))

        numRows <- suppressWarnings(
          tempRows[unlist(typeCols)] |>
            mutate(across(everything(), as.numeric)) |>
            summarise(across(everything(), numBy))
        )

        tempRows <- bind_cols(charRows, numRows)
        tempRows <- select(tempRows, sort(names(tempRows))) |>
          mutate(across(everything(), as.character))


      } else {

        tempRows <- tempRows |>
          summarise(across(everything(), charBy)) |>
          mutate(across(where(is.character), ~na_if(x = ., y = "")))

      }

      tempRows$rn <- min(targetRows)

      tempTab <- tempTab |>
        filter(!rn %in% targetRows) |>
        add_row(tempRows)

    }

    input <- tempTab |>
      arrange(rn) |>
      select(-rn)
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


  return(input)
}