#' Select data chunks from a spreadsheet
#'
#' This function builds a list of those chunks that contain the data.
#' @param input the raw table from which to select clusters
#' @param schema the schema description that is the basis to derive data.
#' @return A list of the rows and columns in the spreadsheet that contain data
#'   plus the header and the data, per cluster.
#' @importFrom checkmate assertDataFrame assertNames
#' @importFrom dplyr group_by ungroup select mutate arrange
#' @importFrom tidyr fill spread gather
#' @importFrom tibble rownames_to_column

selectData <- function(input = NULL, schema = NULL){

  assertDataFrame(x = input)

  # define variables
  clusters <- schema@clusters
  variables <- schema@variables
  header <- schema@header
  clusterID <- clusters$id
  tabDim <- dim(input)
  nrClusters <- max(lengths(clusters))

  if(!is.null(clusters$id)){
    if(clusters$id == "observed"){
      # in case observed variables are cluster variables, get those that contain 'key = "cluster"'
      clusterVar <- sapply(seq_along(variables), function(x){
        if(!is.null(variables[[x]]$key)){
          if(variables[[x]]$key == "cluster"){
            variables[x]
          }
        }
      })
      clusterVar <- unlist(clusterVar, recursive = FALSE)
    } else {
      clusterVar <- rep(variables[clusters$id], nrClusters)
    }
  } else {
    clusterVar <- NULL
  }

  # assume the header is in row 1, if not set
  if(is.null(header$row)){
    header$row <- 1L
    message("  ... you did not set 'header', so I assume it is in the first row.")
  }

  # determine distinct variables
  distinct <- sapply(seq_along(schema@variables), function(x){
    schema@variables[[x]]$dist
  })

  out <- list()
  for(i in 1:nrClusters){
    clusterRows2 <- outRows <- clusters$row[i]:(clusters$row[i]+clusters$height[i] - 1)
    clusterCols2 <- outCols <- clusters$col[i]:(clusters$col[i]+clusters$width[i] - 1)
    thisClust <- input[clusterRows2, clusterCols2]
    clustVar <- clusterVar[[i]]


    if(!is.null(clusterID)){
      # in case clusterID is in column/row without any other values, remove it
      if(clustVar$rel){
        if(clustVar$type == "observed"){
          clusterVal <- clustVar$value
        } else {
          clusterVal <- unlist(thisClust[clustVar$row[i], clustVar$col[i]], use.names = FALSE)

          tempRow <- unlist(thisClust[clustVar$row[i],], use.names = FALSE)
          tempRow[clustVar$col[i]] <- NA
          if(all(is.na(tempRow))){
            outRows <- clusterRows2[-clustVar$row[i]]
          }

          tempCol <- unlist(thisClust[,clustVar$col[i]], use.names = FALSE)
          tempCol[clustVar$row[i]] <- NA
          if(all(is.na(tempCol))){
            outCols <- clusterCols2[-clustVar$col[i]]
          }
        }
      } else {
        if(!is.null(clustVar$value)){
          clusterVal <- clustVar$value
        } else {
          clusterVal <- unlist(input[clustVar$row[i], clustVar$col[i]], use.names = FALSE)

          if(clustVar$row[i] %in% clusterRows2){
            tempRow <- unlist(input[clustVar$row[i], clusterCols2], use.names = FALSE)
            tempRow[which(clusterCols2 %in% clustVar$col[i])] <- NA
          } else {
            tempRow <- "something"
          }
          if(clustVar$col[i] %in% clusterCols2){
            tempCol <- unlist(input[clusterRows2, clustVar$col[i]], use.names = FALSE)
            tempCol[which(clusterRows2 %in% clustVar$row[i])] <- NA
          } else {
            tempCol <- "something"
          }
          if(all(is.na(tempRow))){
            outRows <- outRows[-which(clusterCols2 %in% clustVar$col[i])]
          }
          if(all(is.na(tempCol))){
            outCols <- outCols[-which(clusterRows2 %in% clustVar$row[i])]
          }
        }

      }
    } else {
      clusterVal <- NULL
    }

    # if there is a distinct variable, determine column and rows of it and cut
    # it out as well.
    if(any(distinct)){
      distVar <- schema@variables[distinct][[1]]

      if(!is.null(distVar$value)){
        distVal <- distVar$value
      } else {
        if(!is.null(distVar$col)){
          distCol <- unique(distVar$col)
        } else {
          distCol <- outCols
        }
        if(!is.null(distVar$row)){
          distRow <- distVar$row
        } else {
          distRow <- outRows
        }
        distVal <- input[distRow, distCol]
      }

    } else {
      distVal <- NULL
    }

    # adapt values if header is relative
    if(header$rel){
      headerRows <- clusters$row[i]+header$row-1
    } else {
      headerRows <- header$row
    }

    # new code
    tempData2 <- input[outRows, outCols]
    # remove invalid rows
    toRemove2 <- which(rowSums((is.na(tempData2))) == ncol(tempData2))
    if(header$rel){
      toRemove2 <- clusters$row[i] - headerRows + 1
    } else {
      if(any(headerRows >= clusters$row[i])){
        toRemove2 <- c(toRemove2, headerRows[headerRows >= clusters$row[i]])
      }
    }

    if(length(toRemove2) != 0){
      outRows <- outRows[-toRemove2]
    }
    tempData2 <- input[outRows, clusterCols2]

    # determine header
    tempHeader <- input[headerRows, ]

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

    tempOut <- list(cluster_rows = clusterRows2,
                    data_rows = outRows,
                    cluster_cols = clusterCols2,
                    data_cols = outCols,
                    cluster_var = clustVar,
                    cluster_val = clusterVal,
                    header = tempHeader,
                    data = tempData2,
                    outside = distVal)

    out <- c(out, list(tempOut))
  }

  return(out)
}
