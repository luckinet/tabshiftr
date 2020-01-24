#' Select data chunks from a spreadsheet
#' @param input the raw table from which to select clusters
#' @param clusters the "clusters" slot of a schema description.
#' @importFrom checkmate assertDataFrame assertNames
#' @importFrom dplyr group_by ungroup select mutate arrange
#' @importFrom tidyr fill spread gather
#' @importFrom tibble rownames_to_column
#' @export

selectData <- function(input = NULL, clusters = NULL){

  assertDataFrame(x = input)
  assertNames(x = names(clusters), permutation.of = c("top", "left", "width", "height", "id", "header"))

  out <- list()
  for(i in seq_along(clusters$left)){
    clusterRows <- clusters$top[i]:(clusters$top[i]+clusters$height[i] - 1)
    clusterCols <- clusters$left[i]:(clusters$left[i]+clusters$width[i] - 1)
    temp <- input[clusterRows, clusterCols]

    if(length(clusters$left) == 1){
      temp <- temp[-clusters$header, ]
    }

    # determine header
    header <- input[clusters$header, clusterCols]

    # fill NA to the right side of wide identifying variables (this will add the
    # value to the left of an NA instead of the NA)
    colnames(header) <- formatC(c(1:dim(header)[2]), width = nchar(dim(header)[2]), flag = "0")
    header <- header %>%
      rownames_to_column('rn') %>%
      gather(key, val, -rn) %>%
      group_by(rn) %>%
      fill(val) %>%
      ungroup() %>%
      spread(key, val) %>%
      mutate(rn = as.numeric(rn)) %>%
      arrange(rn) %>%
      select(-rn)
    names(header) <- NULL

    # determine which rows/cols contain valid data
    clustRows <- rep(FALSE, dim(input)[1])
    clustRows[clusterRows] <- TRUE
    clustCols <- rep(FALSE, dim(input)[2])
    clustCols[clusterCols] <- TRUE

    tempOut <- list(header = header,
                    data = temp,
                    cluster_rows = clustRows,
                    cluster_cols = clustCols)

    out <- c(out, list(tempOut))
  }

  return(out)
}