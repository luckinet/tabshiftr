#' Pre-process input table
#'
#' This function groups rows, splices the header into the table and fills
#' missing values where they should not exist.
#' @param schema [\code{character(1)}]\cr the validated schema description of
#'   \code{input}.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @details
#'
#' @return a table where columns and rows are grouped and headers are spliced
#'   into the table.
#' @examples
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

  assertIntegerish(x = header, len = 1, lower = 0, upper = dim(input)[1], any.missing = FALSE, null.ok = TRUE)

  # first splice the header into the table, if it hasn't been read without column names
  if(header != 0L){

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