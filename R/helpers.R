#' Update the formating of a table
#'
#' This function updates the format of a table by applying a schema description
#' to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map
#' @importFrom dplyr row_number

.updateFormat <- function(input = NULL, schema = NULL){

  clusters <- schema@clusters
  variables <- schema@variables
  format <- schema@format

  idVars <- map(.x = seq_along(variables), .f = function(ix){
    if(variables[[ix]]$type == "id"){
      variables[ix]
    }
  })
  idVars <- unlist(idVars, recursive = FALSE)
  obsVars <- map(.x = seq_along(variables), .f = function(ix){
    if(variables[[ix]]$type == "observed"){
      variables[ix]
    }
  })
  obsVars <- unlist(obsVars, recursive = FALSE)

  # set all observed variables to the correct format
  for(i in seq_along(obsVars)){
    theVar <- input[[which(names(obsVars)[i] == names(input))]]

    theVar <- gsub(" ", "", theVar)
    if(!is.null(format$na)){
      theVar[theVar %in% format$na] <- NA
    }
    if(!is.null(format$del)){
      if(format$del == "."){
        format$del <- "[.]"
      }
      theVar <- gsub(format$del, "", theVar)
    }
    if(!is.null(format$dec)){
      if(format$dec == "."){
        format$dec <- "[.]"
      }
      theVar <- gsub(format$dec, ".", theVar)
    }

    # multiply with factor
    theVar <- suppressWarnings(as.numeric(theVar)) * obsVars[[i]]$factor

    input[[which(names(obsVars)[i] == names(input))]] <- theVar
  }

  out <- input %>%
    arrange_at(.vars = names(idVars))

  return(out)

}

#' Match variables
#'
#' This function matches id and observed variables and reshapes them accordingly
#' @param ids list of id variables
#' @param obs list of observed variables
#' @return a symmetric list of variables (all with the same dimensions)
#' @importFrom checkmate assertSetEqual
#' @importFrom purrr reduce map_int map set_names
#' @importFrom tidyr pivot_longer pivot_wider fill separate
#' @importFrom dplyr distinct select bind_cols
#' @importFrom tidyselect all_of everything
#' @importFrom rlang `:=`

.tidyVars <- function(ids = NULL, obs = NULL){

  outIDs <- ids
  outObs <- obs

  uniqueIDs <- map(.x = seq_along(ids), .f = function(ix){
    unique(unlist(ids[[ix]]))
  })
  targetRows <- reduce(lengths(uniqueIDs), `*`)

  idCols <- map(.x = seq_along(ids), .f = function(ix){
    names(ids[[ix]])
  })

  # first, assess whether there are any wide observed variables involved
  widthObs <- map_int(.x = seq_along(obs), .f = function(ix){
    dim(obs[[ix]])[[2]]
  })
  if(!all(1 == widthObs)){
    # if yes, ...

    wideObs <- obs[which(widthObs != 1)]

    # ... identify for each wide observed variable ...
    for(i in seq_along(wideObs)){

      temp <- wideObs[[i]]
      varName <- names(wideObs[i])
      tempDim <- dim(temp)

      # ... the corresponding wide id variables that gives the column names
      wideID <- map(.x = seq_along(idCols), .f = function(ix){
        if(any(idCols[[ix]] %in% names(temp))){
          ids[ix]
        } else {
          # if no names are matching, reuse names of previous iteration
          if(length(names(temp)) == length(idCols[[ix]])){
            ind <- which(lengths(wideID) == length(names(temp)))
            repNames <- tibble(old = names(wideID[[ind]]), new = names(temp))
            tempWideID <- map(.x = seq_along(wideID), .f = function(iy){
              newNames <- repNames$new[which(repNames$old %in% names(wideID[[iy]]))]
              names(wideID[[iy]]) <- newNames
              wideID[iy]
            })
            tempWideID <- unlist(tempWideID, recursive = FALSE)
          }
        }
      })
      wideID <- unlist(wideID, recursive = FALSE)

      # build a tibble for joining with the column names of temp
      wideColnames <- NULL
      for(j in seq_along(wideID)){

        nextColNames <- wideColnames
        wideColnames <- wideID[[j]] %>%
          pivot_longer(cols = everything(),
                       names_to = "name",
                       values_to = names(wideID)[j])

        if(!is.null(nextColNames)){
          wideColnames <- left_join(wideColnames, nextColNames, by = "name") %>%
            fill(everything())
        }
      }

      wideColnames <- wideColnames %>% select(all_of(names(wideID)), everything())

      # find the correct name by joining via the column names
      # targetWide <- which(widthIDs %in% tempDim[2])
      tempColnames <- temp %>% pivot_longer(cols = everything(), names_to = "name", values_to = varName)
      wideNames <- left_join(tempColnames, wideColnames, by = "name") %>%
        select(-all_of(varName)) %>%
        distinct() %>%
        unite(col = "new", !name, sep = "-_-_", na.rm = TRUE) %>%
        pivot_wider(names_from = "name") %>%
        unlist(use.names = FALSE)

      assertSetEqual(x = length(wideNames), y = tempDim[2])
      names(temp) <- wideNames

      # ... all id variables that have the same length
      equalID <- map(.x = seq_along(ids), .f = function(ix){
        if(tempDim[1] == dim(ids[[ix]])[1]){
          bla <- ids[ix]
          names(bla[[1]]) <- names(ids[ix])
          return(bla)
        }
      })
      equalID <- unlist(equalID, recursive = FALSE)
      temp <- bind_cols(temp, equalID, .name_repair = "minimal")

      # and pivot those into longer form
      temp <- pivot_longer(data = temp,
                           cols = all_of(wideNames),
                           names_to = paste0(names(wideID), collapse = "-_-_"),
                           values_to = varName) %>%
        separate(col = paste0(names(wideID), collapse = "-_-_"), into = names(wideID), sep = "-_-_")

      if(i != 1){
        newObs <- suppressMessages(temp %>%
                                     left_join(newObs))
      } else {
        newObs <- temp
      }
    }

    # sort the resulting tibble into the previous lists 'ids' and 'obs'
    idNames <- names(ids)
    outIDs <- map(.x = seq_along(ids), .f = function(ix) {
      newObs[names(ids)[ix]]
    })
    names(outIDs) <- idNames

    obsNames <- names(obs)
    outObs <- map(.x = seq_along(obs), .f = function(ix) {
      newObs[names(obs)[ix]]
    })
    names(outObs) <- obsNames

  }

  # obsNames <- map(.x = seq_along(obs), .f = function(ix){
  #   names(obs[[ix]])
  # })
  # obsNames <- unique(unlist(obsNames))

  # take care of variables that are too wide
  widthsIDs <- map_int(.x = seq_along(outIDs), .f = function(ix){
    dim(outIDs[[ix]])[[2]]
  })
  if(!all(1 == widthsIDs)){

    wideIDs <- ids[which(widthsIDs != 1)]
    newIDs <- map(.x = seq_along(wideIDs), .f = function(ix){
      temp <- wideIDs[ix]
      temp <- pivot_longer(data = temp[[1]], cols = everything(), values_to = paste0(names(temp[[1]]), collapse = " ")) %>%
        select(-name)
    })
    newIDs <- set_names(x = newIDs, nm = names(wideIDs))
    outIDs[which(widthsIDs != 1)] <- newIDs

  }

  # take care of variables that are not long enough
  lengthIDs <- map_int(.x = seq_along(outIDs), .f = function(ix){
    dim(outIDs[[ix]])[[1]]
  })
  # lengthObs <- map_int(.x = seq_along(obs), .f = function(ix){
  #   dim(obs[[ix]])[[1]]
  # })

  if(any(lengthIDs != targetRows)){

    tooShort <- which(lengthIDs != targetRows)
    for(i in seq_along(tooShort)){
      ind <- tooShort[i]
      if(lengthIDs[ind] == 1){
        outIDs[[ind]] <- tibble(!!names(ids[[ind]]) := rep(ids[[ind]][[1]], targetRows))
      }
    }
  }

  return(c(outIDs, outObs))

}

#' Evaluate .find constructs
#'
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param col [\code{list(2)}]\cr the output of the respective .find construct
#'   used to match in columns.
#' @param row [\code{list(2)}]\cr the output of the respective .find construct
#'   used to match in rows.
#' @return the columns or rows where the evaluated position
#' @importFrom checkmate assertNumeric
#' @importFrom rlang eval_tidy
#' @importFrom purrr map_int map_lgl
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr select mutate across
#' @importFrom stringr str_count

.eval_find <- function(input = NULL, col = NULL, row = NULL){


  # in case to look for columns
  if(!is.null(col)){
    term <- eval_tidy(col$by)

    if(is.function(term)){

      # this should probably be written so that header rows are excluded so that
      # the columns can have their corret data type against which the functions
      # can test

      # if(!is.null(varProp$row)){
      #   subset <- input[unique(varProp$row),]
      # } else {
      #   subset <- input
      # }
      #
      # # make a subset table that contains numbers when possible
      # subset <- subset %>%
      #   mutate(across(everything(), function(x) replace_na(x, 0))) %>%
      #   mutate(across(.cols = where(function(x) suppressWarnings(!anyNA(as.numeric(x)))), .fns = as.numeric))
      #
      # cols <- map_lgl(.x = 1:dim(input)[2], .f = function(ix){
      #   map(subset[[ix]], term)[[1]]
      # })


    } else {
      cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
        str_count(string = paste(input[[ix]], collapse = " "), pattern = term)
      })
    }
    out <- rep(seq_along(cols), cols)

  }

  # in case to look for rows
  if(!is.null(row)){
    term <- eval_tidy(row$by)

    if(is.function(term)){

      if(!is.null(row$col)){
        assertNumeric(x = row$col, len = 1, any.missing = FALSE)
        subset <- input[,unique(row$col)]
      } else {
        subset <- input
      }

      # make a subset table that contains numbers when possible
      subset <- subset %>%
        rownames_to_column() %>%
        pivot_longer(-rowname, 'variable', 'value') %>%
        pivot_wider(variable, rowname) %>%
        select(-variable) %>%
        mutate(across(.cols = where(function(x) suppressWarnings(!anyNA(as.numeric(x)))), .fns = as.numeric))

      rows <- map_int(.x = 1:dim(subset)[2], .f = function(ix){
        map(subset[,ix], term)[[1]]
      })
    } else {
      rows <- map_int(.x = 1:dim(input)[1], .f = function(ix){
        if(!is.null(row$col)){
          if(!is.na(input[ix,row$col])){
            lookup <- unlist(input[ix,row$col], use.names = FALSE)
          } else {
            lookup <- ""
          }
        } else {
          lookup <-input[ix,]
        }
        str_count(string = paste(lookup, collapse = " "), pattern = term)
      })
    }
    out <- rep(seq_along(rows), rows)

  }

  return(out)

}


#' Test for a valid table
#'
#' This function is a collection of expectations which ensure that the output of
#' \code{\link{reorganise}} is formally and contentwise correct. It is used in
#' the tests of this package.
#' @param x a table to test.
#' @param units the number of units in the output table (from 1 to 3)
#' @param variables the variables that should be in the output table (either
#'   "harvested" or "production")
#' @param groups whether or not groups are in the test table.
#' @return Either an error message of the invalid expectations, or the output of
#'   the last successful expectation.
#' @importFrom testthat expect_identical
#' @importFrom checkmate expect_names expect_tibble expect_list assertChoice

.expect_valid_table <- function(x = NULL, units = 1, variables = NULL, groups = FALSE){

  assertChoice(x = units, choices = c(1:3))
  assertChoice(x = variables, choices = c("harvested", "production"), null.ok = TRUE)

  if(units == 1){

    expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1"))
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean"))
    if(is.null(variables)){
      expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 5)
      expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
      expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211))
      expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212))
    } else {
      expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 4)
      expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", variables) )
      if(variables == "harvested") expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211))
      if(variables == "production") expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212))
    }


  } else if(units == 2){

    expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
    if(is.null(variables)){
      expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 5)
      expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
      expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211))
      expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212))
    } else {
      expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 4)
      expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", variables) )
      if(variables == "harvested") expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211))
      if(variables == "production") expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212))
    }


  } else if(units == 3){

    if(groups){
      expect_identical(object = x$territories, expected = c("group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 2", "group 2", "group 2", "group 2"))
      expect_identical(object = x$sublevel, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2", "unit 3", "unit 3", "unit 3", "unit 3"))
    } else {
      expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2", "unit 3", "unit 3", "unit 3", "unit 3"))
    }
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
    if(is.null(variables)){
      if(groups){
        expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 6)
        expect_names(x = colnames(x), permutation.of = c("territories", "sublevel", "year", "commodities", "harvested", "production") )
      } else {
        expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 5)
        expect_names(x = colnames(x), permutation.of = c("territories", "year", "commodities", "harvested", "production") )
      }
      expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211, 3121, 3111, 3221, 3211))
      expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212, 3122, 3112, 3222, 3212))
    } else {
      expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 4)
      expect_names(x = colnames(x), permutation.of = c("territories", "year", "commodities", variables) )
      if(variables == "harvested") expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211, 2121, 2111, 2221, 2211, 3121, 3111, 3221, 3211))
      if(variables == "production") expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212, 2122, 2112, 2222, 2212, 3122, 3112, 3222, 3212))
    }
  }
}