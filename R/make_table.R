#' Make a tables from a pdf file
#'
#' extract information from a PDF table and turn it into a table
#' @param pdf [character(1)][character]\cr
#' @param pages [integerish(1)][integer]\cr the range of pages to process, by
#'   default it's all the pages.
#' @param rows [character(1)][character]|[integerish(1)][integer]\cr the way to estimate rows, possible
#'   values are "canvas", "word_height" (the most frequent height) or an integer
#'   value of the number of rows.
#' @param cols [character(1)][character]|[integerish(1)][integer]\cr the way to estimate columns, possible
#'   values are "canvas", "median" or an integer value of the number of columns.
#' @param use.centroid [logical(1)][logical]\cr whether or not to identify the
#'   position of text elements by it's centroid, or the minimum x and y value.
#' @param merge [character(1)][character]\cr symbol by which to merge when several
#'   text boxes fall into the same cell.
#' @return a rectangular table
#' @examples
#' # syr_2000 <- make_tables(pdf = "2000_Rain and land use.pdf")
#' # syr_2000[[4]] <- make_tables(pdf = "2000_Rain and land use.pdf",
#' #                              pages = 4, use.centroid = TRUE)
#' @importFrom checkmate assertFileExists testCharacter assertChoice
#'   assertIntegerish
#' @importFrom dplyr between mutate group_by summarise arrange desc
#' @importFrom stringr str_split
#' @importFrom purrr map_int
#' @importFrom tibble tibble
#' @importFrom pdftools pdf_data pdf_text
#' @export

make_tables <- function(pdf = NULL, pages = NULL, rows = "canvas",
                        cols = "canvas", use.centroid = TRUE, merge = "_-_"){

  assertFileExists(x = pdf, access = "r")
  if(testCharacter(rows)){
    assertChoice(x = rows, choices = c("canvas", "word_height"))
  } else {
    assertIntegerish(x = rows, len = 1)
  }
  if(testCharacter(cols)){
    assertChoice(x = cols, choices = c("canvas", "median"))
  } else {
    assertIntegerish(x = cols, len = 1)
  }

  to_table <- function(x, cls, rws, use.centroid, widths = NULL) {

    xRange <- range(x$x)
    yRange <- range(x$y)

    if(is.null(widths)){
      xBreaks <- (xRange[2] - xRange[1]) / cls
      xBreaks <- seq(from = xRange[1], to = xRange[2], by = xBreaks)
    } else {
      # xBreaks <- widths/sum(widths) * (xRange[2] - xRange[1])
      # xBreaks <- cumsum(c(xRange[1], xBreaks))
    }

    yBreaks <- (yRange[2] - yRange[1]) / rws
    yBreaks <- seq(from = yRange[1], to = yRange[2], by = yBreaks)

    if(use.centroid){
      x$x <- (x$x + x$width/2)
      x$y <- (x$y + x$height/2)
    }

    x$col <- x$row <- NA_integer_
    for(i in 1:(length(xBreaks)-1)){
      pos <- x$x %>% between(xBreaks[i], xBreaks[i+1])
      x$col[pos] <- i
    }
    for(i in 1:(length(yBreaks)-1)){
      pos <- x$y %>% between(yBreaks[i], yBreaks[i+1])
      x$row[pos] <- i
    }

    # ... here

    out <- matrix(nrow = rws, ncol = cls, data = NA)

    for(i in seq_along(x$text)){
      out[x$row[i], x$col[i]] <- paste0(c(na.omit(out[x$row[i], x$col[i]]), x$text[i]), collapse = merge)
    }

    out <- as_tibble(out, .name_repair = "minimal")
    names(out) <- paste0("X", 1:dim(out)[2])

    return(out)
  }
  # pdf <- "2000_Rain and land use.pdf"
  inData <- pdf_data(pdf)
  inText <- pdf_text(pdf)

  if(is.null(pages)){
    pages <- seq_along(inData)
  } else {
    assertIntegerish(x = pages, lower = 1, upper = length(inData), any.missing = FALSE)
  }

  out <- NULL
  for(i in pages){

    input <- inData[[i]] %>%
      mutate(x_centroid = x + width/2,
             y_centroid = y + height/2)

    if(any(dim(input) == 0)){
      next
    }

    # determine the number of columns by reading the data in with pdf_text and then splicing off that is not whitespace
    theRows <- str_split(inText[[i]], "\n")[[1]]
    # tab <- tibble(row = theRows)
    nCols <- map_int(seq_along(theRows), function(ix){
      temp <- str_split(theRows[ix], " ")[[1]]
      length(temp[which(temp != "")])
    })
    medianCols <- quantile(nCols, probs = 0.5)
    nCols <- tibble(cols = nCols) %>%
      group_by(cols) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    nrColumns <- nCols$cols[1]

    # determine the number of rows by taking the average word height and dividing the range by that
    wordHeight <- input %>%
      group_by(height) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    wordHeight <- wordHeight$height[1]
    tabHeight <- range(input$y)
    estRows <- ceiling((tabHeight[2] - tabHeight[1]) / wordHeight)

    if(rows == "canvas"){
      rows <- length(theRows)
    } else if(rows == "word_height"){
      rows <- estRows
    }

    if(cols == "canvas"){
      cols <- nrColumns
    } else if(cols == "median"){
      cols <- medianCols
    }

    tab <- to_table(input, cols, rows, use.centroid = FALSE)

    # # then get the number of chars per column
    # widths <- tab %>%
    #   mutate(across(everything(), str_length)) %>%
    #   summarise(across(everything(), median, na.rm = T))
    #
    #
    # # finally allocate into table with adjusted column widths
    # tab <- to_table(input, cols, rows, use.centroid, widths)
# this should go inside the to_table function

    out <- c(out, list(tab))
  }



  # columns <- 12
  # rows <- 50
  # input <- input %>%
  #   arrange(y_centroid, x_centroid)

  # input$x_dist <- c(input$x[-1], tail(input$x, 1)) - input$x
  # input$y_dist <- c(input$y[-1], tail(input$y, 1)) - input$y

  # ggplot(input, aes(x_centroid, y_centroid)) +
  #   geom_point() +
  #   scale_y_reverse()
  # ggplot(input, aes(x_centroid)) +
  #   geom_histogram()


  # get the number of rows that can be discarded
  # emptyRows <- rowSums(is.na(lat)) == dim(lat)[2]
  # lat <- to_lattice(input, columns, rows - sum(emptyRows), use.centroid = TRUE)

  return(out)
}


#' Make a tables from a pdf file
#'
#' extract information from a PDF table and turn it into a table
#' @param tab [data.frame(.)][data.frame]\cr
#' @param merge [character(1)][character]\cr symbol by which to merge when several
#'   text boxes fall into the same cell.
#' @return diagnostic plot of a table.
#' @examples
#' # please check the vignette for examples
#' @importFrom checkmate assertFileExists
#' @importFrom dplyr between mutate group_by summarise arrange desc
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_rect scale_y_reverse scale_colour_manual
#'   labs ggtitle
#' @export

plot_table <- function(tab, merge = "_-_"){

  if(is.matrix(tab)){
    data <- as.vector(tab)
  } else if(is.data.frame(tab)){
    data <- unlist(tab)
  }

  out <- tibble(x = rep(x = 1:dim(tab)[2], each = dim(tab)[1]),
         y = rep(x = 1:dim(tab)[1], times = dim(tab)[2]),
         text = data,
         col = if_else(!is.na(text), if_else(grepl(merge, text), ">= 2", "1"), "0")) %>%
    arrange(col) %>%
    ggplot(aes(x, y, col = col)) +
    geom_rect(mapping = aes(xmin = x-.5, xmax = x+.5, ymin = y-.2, ymax = y+.2)) +
    scale_y_reverse() +
    scale_colour_manual(values = c("0" = "deeppink", "1" = "green", ">= 2" = "blue")) +
    ggtitle(tabName) +
    labs(col = 'items')

  plot(out)
  invisible(tab)

}
