#' get the names from a schema description
#' @param temp a temporary table for which to derive names.
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

  # if a variable if outside of a cluster, remove its name
  if(!is.null(meta$cluster$outside_cluster)){
    outNames <- outNames[-which(outNames %in% meta$cluster$outside_cluster)]
  }

  # if a variable is the cluster ID, remove its name
  if(!is.null(meta$cluster$cluster_id)){
    outNames <- outNames[-which(outNames %in% meta$cluster$cluster_id)]
  }

  # if a header is used, replace values in there with variable names and provide it as outNames
  if(!is.null(meta$table$header)){
    outNames <- temp %>% slice(1) %>% unlist()
    for(i in seq_along(meta$table$tidy_cols)){
      outNames[meta$table$tidy_cols[i]] <- meta$table$tidy[i]
    }
  }

  # if there are names in several rows, merge them
  if(!is.null(meta$table$gather_into)){
    if(!is.null(meta$cluster$merge_rows)){
      outNames <- temp %>%
        t() %>%
        as_tibble() %>%
        select(meta$cluster$merge_rows) %>%
        unite(col = "name", sep = "-_-_", na.rm = TRUE) %>%
        unlist()

      # make sure that tidy variables actually have correct names
      for(i in seq_along(meta$table$tidy)){
        if(!is.null(meta$cluster$cluster_id)){
          if(!meta$cluster$cluster_id %in%  meta$table$tidy[i]){
            outNames[meta$table$tidy_cols[i]] <- meta$table$tidy[i]
          }
        } else {
          outNames[meta$table$tidy_cols[i]] <- meta$table$tidy[i]
        }

      }
    }
  }


  return(outNames)

}