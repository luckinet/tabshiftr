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

.selectData <- function(input = NULL, schema = NULL){

  assertDataFrame(x = input)

  # define variables
  clusters <- schema@clusters
  variables <- schema@variables
  header <- schema@header
  clusterID <- clusters$id
  groupID <- clusters$group
  tabDim <- dim(input)
  nrClusters <- max(lengths(clusters))

  if(!is.null(clusterID)){
    if(clusterID == "observed"){
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
      clusterVar <- rep(variables[clusterID], nrClusters)
    }
  } else {
    clusterVar <- NULL
  }

  if(!is.null(groupID)){
    groupVar <- rep(variables[clusters$group], nrClusters)
  } else {
    groupVar <- NULL
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
  setNA <- list()
  for(i in 1:nrClusters){
    clusterRows <- outRows <- clusters$row[i]:(clusters$row[i]+clusters$height[i] - 1)
    clusterCols <- outCols <- clusters$col[i]:(clusters$col[i]+clusters$width[i] - 1)
    thisClust <- input[clusterRows, clusterCols]
    clustVar <- clusterVar[[i]]
    parVar <- groupVar[[i]]

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
            outRows <- clusterRows[-clustVar$row[i]]
          }

          tempCol <- unlist(thisClust[,clustVar$col[i]], use.names = FALSE)
          tempCol[clustVar$row[i]] <- NA
          if(all(is.na(tempCol))){
            outCols <- clusterCols[-clustVar$col[i]]
          }
        }
      } else {
        if(!is.null(clustVar$value)){
          clusterVal <- clustVar$value
        } else {
          clusterVal <- unlist(input[clustVar$row[i], clustVar$col[i]], use.names = FALSE)

          # setNA$row <- clustVar$row
          # setNA$col <- clustVar$col

          if(clustVar$row[i] %in% clusterRows){
            tempRow <- unlist(input[clustVar$row[i], clusterCols], use.names = FALSE)
            tempRow[which(clusterCols %in% clustVar$col[i])] <- NA
          } else {
            tempRow <- "something"
          }
          if(clustVar$col[i] %in% clusterCols){
            tempCol <- unlist(input[clusterRows, clustVar$col[i]], use.names = FALSE)
            tempCol[which(clusterRows %in% clustVar$row[i])] <- NA
          } else {
            tempCol <- "something"
          }
          if(all(is.na(tempRow))){
            outRows <- outRows[-which(clusterCols %in% clustVar$col[i])]
          }
          if(all(is.na(tempCol))){
            outCols <- outCols[-which(clusterRows %in% clustVar$row[i])]
          }
        }

      }
    } else {
      clusterVal <- NULL
    }

    if(!is.null(groupID)){
      groupVal <- unlist(input[parVar$row[i], parVar$col[i]], use.names = FALSE)

      setNA$row <- c(setNA$row, parVar$row)
      setNA$col <- c(setNA$col, parVar$col)
    } else {
      groupVal <- NULL
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
    tempIn <- input
    tempIn[unique(setNA$row), unique(setNA$col)] <- NA
    tempData2 <- tempIn[outRows, outCols]
    # remove invalid rows
    removeRows <- NULL
    if(header$rel){
      removeRows <- headerRows - clusters$row[i] + 1
    } else {
      if(any(headerRows >= clusters$row[i])){
        removeRows <- c(removeRows, which(outRows %in% headerRows[headerRows >= clusters$row[i]]))
      }
    }
    removeRows <- c(removeRows, which(rowSums(is.na(tempData2)) == ncol(tempData2)))
    removeCols <- which(colSums(is.na(tempData2)) == nrow(tempData2))

    if(length(removeRows) != 0){
      outRows <- outRows[-removeRows]
    }
    if(length(removeCols) != 0){
      outCols <- outCols[-removeCols]
    }
    tempData2 <- input[outRows, outCols]

    # determine header
    tempHeader <- input[headerRows, ]

    # fill NA to the right side of wide identifying variables (this will replace
    # the NA with the value to the left of that NA)
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

    tempOut <- list(cluster_rows = clusterRows,
                    data_rows = outRows,
                    cluster_cols = clusterCols,
                    data_cols = outCols,
                    cluster_var = clustVar,
                    cluster_val = clusterVal,
                    group_val = groupVal,
                    header = tempHeader,
                    data = tempData2,
                    outside = distVal)

    out <- c(out, list(tempOut))
  }

  return(out)
}
