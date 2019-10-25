#' get the names from a schema description
#' @param temp a temporary table for which to derive names
#' @param meta the output of \code{getMetadata} as basis to derive names.
#' @importFrom checkmate assertDataFrame assertList assertNames
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @export

getNames <- function(temp = NULL, meta = NULL){

  assertDataFrame(x = temp)
  assertList(x = meta, len = 3)
  assertNames(x = names(meta), permutation.of = c("cluster", "var_type", "table"))

  allNames <- outNames <- unique(c(meta$var_type$ids, meta$var_type$vals))

  if(!is.null(meta$cluster$outside_cluster)){
    outNames <- outNames[-which(outNames %in% meta$cluster$outside_cluster)]
  }

  if(!is.null(meta$cluster$cluster_id)){
    outNames <- outNames[-which(outNames %in% meta$cluster$cluster_id)]
  }

  if(!is.null(meta$table$header) & !all(allNames %in% meta$table$tidy)){
    outNames <- meta$table$header
  }

  if(!is.null(meta$table$gather_into)){
    if(!is.null(meta$cluster$merge_rows)){
      outNames <- temp %>%
        t() %>%
        as_tibble() %>%
        select(meta$cluster$merge_rows) %>%
        unite(col = "name", sep = "-_-_", na.rm = TRUE) %>%
        unlist()
    }
  }

  return(outNames)

}