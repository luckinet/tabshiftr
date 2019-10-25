#' Select data chunks from a spreadsheet
#' @param input the raw table from which to select clusters
#' @param clusters the "clusters" slot of a schema description.
#' @importFrom checkmate assertDataFrame assertNames
#' @export

selectData <- function(input = NULL, clusters = NULL){

  assertDataFrame(x = input)
  assertNames(x = names(clusters), permutation.of = c("top", "left", "width", "height", "id", "header"))

  out <- list()
  for(i in seq_along(clusters$left)){
    clusterRows <- clusters$top[i]:(clusters$top[i]+clusters$height[i] - 1)
    clusterCols <- clusters$left[i]:(clusters$left[i]+clusters$width[i] - 1)
    temp <- input[clusterRows, clusterCols]

    # determine which rows/cols contain valid data
    clustRows <- rep(FALSE, dim(input)[1])
    clustRows[clusterRows] <- TRUE
    clustCols <- rep(FALSE, dim(input)[2])
    clustCols[clusterCols] <- TRUE

    tempOut <- list(data = temp,
                    cluster_rows = clustRows,
                    cluster_cols = clustCols)

    out <- c(out, list(tempOut))
  }

  return(out)
}