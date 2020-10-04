#' Determine the names from a schema description
#'
#' This function determines the specific column names that are required in the
#' process of reshaping.
#' @param header the header from which to derive names.
#' @param meta the output of \code{getMetadata} as basis to derive names.
#' @return A vector of column names.
#' @importFrom checkmate assertCharacter assertList assertNames
#' @importFrom stats na.omit
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr is_character
#' @importFrom dplyr select na_if
#' @importFrom stringr str_c
#' @importFrom tidyr unite

getNames <- function(header = NULL, meta = NULL){

  assertDataFrame(x = header)
  assertList(x = meta, len = 4)

  if(meta$header$merge){
    newHead <- lapply(seq_along(header), function(x){
      paste0(na.omit(header[[x]]), collapse = " ")
    })
    header <- as_tibble(newHead, .name_repair = "minimal")
  }

  if(!is.null(meta$table$gather_into)){
    theNames <- suppressMessages(
      header %>%
        t() %>%
        as_tibble(.name_repair = "unique") %>%
        mutate_if(is_character, list(~na_if(.,""))) %>%
        fill(1) %>%
        select(meta$header$cols) %>%
        unite(col = "name", sep = "-_-_", na.rm = TRUE) %>%
        unlist())
  } else {
    theNames <- header %>%
      unlist()
  }

  # check that no name is duplicated
  theNames <- tibble(name = theNames) %>%
    group_by(name) %>%
    mutate(count = seq_along(name),
           count = ifelse(count == 1, "", str_c(".", count))) %>%
    ungroup() %>%
    mutate(name = str_c(name, count)) %>%
    select(-count) %>%
    unlist()

  # make sure that tidy variables actually have correct names
  targetCols <- meta$cluster$cluster_cols[meta$table$tidy_cols]
  for(j in seq_along(theNames)){

    testVar <- theNames[j]

    # if the recent name is not from a tidy column, remove it ...
    if(!j %in% targetCols){
      next
    }
    isClust <- isOut <- FALSE
    if(!is.null(meta$cluster$cluster_id)){
      if(testVar == meta$cluster$cluster_id){
        isClust <- TRUE
      }
    }
    if(!is.null(meta$cluster$outside_cluster)){
      if(any(testVar %in% meta$cluster$outside_cluster)){
        isOut <- TRUE
      }
    }
    if(!isClust & !isOut){
      theNames[j] <- meta$table$tidy[which(targetCols == j)]
    }
  }

  names(theNames) <- NULL
  return(theNames)

}
