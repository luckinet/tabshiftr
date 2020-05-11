#' Select data chunks from a spreadsheet
#'
#' This function builds a list of those chunks that contain the data.
#' @param input the raw table from which to select clusters
#' @param clusters the "clusters" slot of a schema description.
#' @param header the "header" slot of a schema description.
#' @return A list of the rows and columns in the spreadsheet that contain data
#'   plus the header and the data, per cluster.
#' @importFrom checkmate assertDataFrame assertNames
#' @importFrom dplyr group_by ungroup select mutate arrange
#' @importFrom tidyr fill spread gather
#' @importFrom tibble rownames_to_column

selectData <- function(input = NULL, clusters = NULL, header = NULL){

  assertDataFrame(x = input)

  # assume the header is in row 1, if not set
  if(is.null(header$row)){
    header$row <- 1L
    message("  ... you did not set 'header', so I assume it is in the first row.")
  }

  out <- list()
  nrClusters <- max(lengths(clusters))
  for(i in 1:nrClusters){
    clusterRows <- clusters$row[i]:(clusters$row[i]+clusters$height[i] - 1)
    clusterCols <- clusters$col[i]:(clusters$col[i]+clusters$width[i] - 1)
    tempCols <- input[, clusterCols]

    # determine which rows/cols contain valid data
    clustRows <- rep(FALSE, dim(input)[1])
    clustRows[clusterRows] <- TRUE
    clustCols <- rep(FALSE, dim(input)[2])
    clustCols[clusterCols] <- TRUE

    # adapt values if header is relative
    if(header$rel){
      tempHeader <- clusters$row[i]+header$row-1
    } else {
      tempHeader <- header$row
    }

    # remove rows from 'temp' that are header rows within a cluster
    if(any(tempHeader >= clusters$row[i])){
      toRemove <- tempHeader[tempHeader >= clusters$row[i]]
      clustRows[toRemove] <- FALSE
    }
    # and remove rows that contain only NAs
    clustRows <- clustRows & rowSums(is.na(tempCols)) != ncol(tempCols)
    temp <- tempCols[clustRows, ]

    # determine header
    tempHeader <- input[tempHeader, clusterCols]

    # fill NA to the right side of wide identifying variables (this will add the
    # value to the left of an NA instead of the NA)
    colnames(tempHeader) <- formatC(c(1:dim(tempHeader)[2]), width = nchar(dim(tempHeader)[2]), flag = "0")
    tempHeader <- tempHeader %>%
      rownames_to_column('rn') %>%
      gather(key, val, -rn) %>%
      group_by(rn) %>%
      fill(val) %>%
      ungroup() %>%
      spread(key, val) %>%
      mutate(rn = as.numeric(rn)) %>%
      arrange(rn) %>%
      select(-rn)
    names(tempHeader) <- NULL

    tempOut <- list(cluster_rows = clustRows,
                    cluster_cols = clustCols,
                    header = tempHeader,
                    data = temp)

    out <- c(out, list(tempOut))
  }

  return(out)
}
