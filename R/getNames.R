#' get the names from a schema description
#' @param header the header from which to derive names.
#' @param meta the output of \code{getMetadata} as basis to derive names.
#' @importFrom checkmate assertCharacter assertList assertNames
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @export

getNames <- function(header = NULL, meta = NULL){

  assertDataFrame(x = header)
  assertList(x = meta, len = 3)
  assertNames(x = names(meta), permutation.of = c("cluster", "var_type", "table"))

  if(!is.null(meta$table$gather_into)){
      theNames <- header %>%
        t() %>%
        as_tibble(.name_repair = "unique") %>%
        select(meta$cluster$header) %>%
        unite(col = "name", sep = "-_-_", na.rm = TRUE) %>%
        unlist()
  } else {
    theNames <- header
  }

  # make sure that tidy variables actually have correct names
  for(j in seq_along(meta$table$tidy)){
    testVar <- meta$table$tidy[j]
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
      theNames[meta$table$tidy_cols[j]] <- meta$table$tidy[j]
    }
  }


  return(theNames)

}