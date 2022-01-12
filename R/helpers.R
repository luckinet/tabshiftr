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
#' @param clust list of cluster variables
#' @param grp list of group variables
#' @return a symmetric list of variables (all with the same dimensions)
#' @importFrom checkmate assertSetEqual
#' @importFrom purrr reduce map_int map set_names
#' @importFrom tidyr pivot_longer pivot_wider fill separate
#' @importFrom dplyr distinct select bind_cols if_any
#' @importFrom tidyselect all_of everything
#' @importFrom rlang `:=`

.tidyVars <- function(ids = NULL, obs = NULL, clust = NULL, grp = NULL){

  outIDs <- ids
  outObs <- obs

  uniqueIDs <- map(.x = seq_along(ids), .f = function(ix){
    unique(unlist(ids[[ix]]))
  })
  # targetRows <- reduce(lengths(uniqueIDs), `*`)

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

      if(varName == "listed"){

        obsNames <- unique(outObs$listed$key)
        idNames <- names(ids)

        equalID <- map(.x = seq_along(outIDs), .f = function(ix){
          if(tempDim[1] == dim(ids[[ix]])[1]){
            set_names(outIDs[[ix]], idNames[ix])
          } else if(all(dim(ids[[ix]]) == c(1, 1))){
            set_names(list(tibble(!!names(ids[ix]) := rep(ids[[ix]][[1]], tempDim[1]))), names(ids[ix]))
          }
        })

        if(length(wideID) > 1){
          wideNames <- wideColnames %>%
            select(all_of(names(wideID)), everything()) %>%
            unite(col = "new", !name, sep = "-_-_", na.rm = TRUE) %>%
            pivot_wider(names_from = "name", values_from = "new") %>%
            unlist(use.names = FALSE)
          wideName <- paste0(names(wideID), collapse = "-_-_")
        } else {
          wideNames <- unlist(wideID, use.names = FALSE)
          wideName <- names(wideID)
        }

        tempObs <- outObs
        if(!is.null(wideID)){
          names(tempObs$listed) <- c("key", wideNames)
          newObs <- bind_cols(c(equalID, tempObs), .name_repair = "minimal") %>%
            pivot_longer(cols = all_of(wideNames), names_to = wideName)
          valueNames <- "value"
        } else {
          newObs <- bind_cols(c(equalID, tempObs), .name_repair = "minimal")
          valueNames <- names(newObs)[!names(newObs) %in% c(idNames, "key")]
        }

        dupObs <- newObs %>%
          pivot_wider(names_from = "key",
                      values_from = all_of(valueNames),
                      values_fn = length) %>%
          mutate(row = row_number()) %>%
          filter(if_any(all_of(obsNames), ~ . != 1))

        if(dim(dupObs)[1] != 0){
          warning("rows(", paste0(dupObs$row, collapse = ", "), ") are duplicated.")
        }

        newObs <- newObs %>%
          pivot_wider(names_from = "key",
                      values_from = all_of(valueNames),
                      values_fn = list)

        if(length(wideID) > 1){
          newObs <- newObs %>%
            separate(col = wideName,
                     into = names(wideID),
                     sep = "-_-_")
        }

      } else {

        obsNames <- names(obs)
        idNames <- names(ids)

        wideColnames <- wideColnames %>% select(all_of(names(wideID)), everything())

        # find the correct name by joining via the column names
        tempColnames <- temp %>% pivot_longer(cols = everything(), names_to = "name", values_to = varName)
        wideNames <- left_join(tempColnames, wideColnames, by = "name") %>%
          select(-all_of(varName)) %>%
          distinct() %>%
          unite(col = "new", !name, sep = "-_-_", na.rm = TRUE) %>%
          pivot_wider(names_from = "name", values_from = "new") %>%
          unlist(use.names = FALSE)

        assertSetEqual(x = length(wideNames), y = tempDim[2])
        names(temp) <- wideNames

        # ... all id variables that have the same length
        equalID <- map(.x = seq_along(ids), .f = function(ix){
          if(tempDim[1] == dim(ids[[ix]])[1]){
            bla <- ids[ix]
            names(bla[[1]]) <- names(ids[ix])
            return(bla)
          } else if(all(dim(ids[[ix]]) == c(1, 1))){
            set_names(list(tibble(!!names(ids[ix]) := rep(ids[[ix]][[1]], tempDim[1]))), names(ids[ix]))
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

    }

    targetRows <- dim(newObs)[1]

    # sort the resulting tibble into the previous lists 'ids' and 'obs'
    outIDs <- map(.x = seq_along(idNames), .f = function(ix) {
      newObs[idNames[ix]]
    })
    names(outIDs) <- idNames

    outObs <- map(.x = seq_along(obsNames), .f = function(ix) {
      newObs[obsNames[ix]]
    })
    names(outObs) <- obsNames

  } else {
    targetRows <- unique(map_int(.x = seq_along(outObs), .f = function(ix){
      dim(outObs[[ix]])[1]
    }))
    assertIntegerish(x = targetRows, len = 1)
  }

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

  if(any(lengthIDs != targetRows)){

    tooShort <- which(lengthIDs != targetRows)
    for(i in seq_along(tooShort)){
      ind <- tooShort[i]
      if(lengthIDs[ind] == 1){
        outIDs[[ind]] <- tibble(!!names(ids[[ind]]) := rep(ids[[ind]][[1]], targetRows))
      }
    }
  }

  outGrp <- NULL
  if(!is.null(grp)){
    dims <- dim(grp[[1]])

    nrRows <- targetRows * length(unique(unlist(grp)))

    if(all(dims == 1)){
      temp <- tibble(X = rep(unlist(grp, use.names = FALSE), nrRows))
    } else {
      temp <- tibble(X = unlist(grp, use.names = FALSE))
    }
    outGrp <-  set_names(x = list(temp), nm = names(grp))

  }

  outClust <- NULL
  if(!is.null(clust)){
    if(is.list(clust)){
      dims <- dim(clust[[1]])

      nrRows <- targetRows * length(unique(unlist(clust)))

      if(all(dims == 1)){
        temp <- tibble(X = rep(unlist(clust, use.names = FALSE), nrRows))
      } else {
        temp <- tibble(X = unlist(clust, use.names = FALSE))
      }
      outClust <-  set_names(x = list(temp), nm = names(clust))

    }
  }

  return(c(outGrp, outClust, outIDs, outObs))

}

#' Evaluate .find constructs
#'
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param col [\code{list(2)}]\cr the output of the respective .find construct
#'   used to match in columns.
#' @param row [\code{list(2)}]\cr the output of the respective .find construct
#'   used to match in rows.
#' @return the columns or rows of the evaluated position
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
    if(is.list(col)){
      term <- eval_tidy(col$by)

      if(is.function(term)){

        if(!is.null(col$row)){
          assertNumeric(x = col$row, len = 1, any.missing = FALSE)
          subset <- input[unique(col$row),]
        } else {
          subset <- input
        }

        # make a subset table that contains numbers when possible
        subset <- subset %>%
          mutate(across(.cols = where(function(x) suppressWarnings(!anyNA(as.numeric(x)))), .fns = as.numeric))

        cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
          map(subset[,ix], term)[[1]] & !is.na(subset[,ix])[[1]]
        })

      } else {
        cols <- map_int(.x = 1:dim(input)[2], .f = function(ix){
          str_count(string = paste(input[[ix]], collapse = " "), pattern = term)
        })
      }
      out <- rep(seq_along(cols), cols)
    }

  }

  # in case to look for rows
  if(!is.null(row)){
    if(is.list(row)){
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
          pivot_longer(cols = -rowname, names_to = 'variable', values_to = 'value') %>%
          pivot_wider(id_cols = variable, names_from = rowname, values_from = value) %>%
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

    if(groups){
      expect_identical(object = x$region, expected = c("group 1", "group 1", "group 1", "group 1"))
    }
    expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1"))
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean"))
    if(is.null(variables)){
      if(groups){
        expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 6)
        expect_names(x = colnames(x), permutation.of = c("region", "territories", "year", "commodities", "harvested", "production") )
      } else {
        expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 5)
        expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
      }
      expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211))
      expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212))
    } else {
      expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 4)
      expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", variables) )
      if(variables == "harvested") expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211))
      if(variables == "production") expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212))
    }

  } else if(units == 2){

    if(groups){
      expect_identical(object = x$region, expected = c("group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1"))
      expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
    } else {
      expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2"))
    }
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
    if(is.null(variables)){
      if(groups){
        expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 6)
        expect_names(x = colnames(x), permutation.of = c("region", "territories", "year", "commodities", "harvested", "production") )
      } else {
        expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 5)
        expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
      }

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
      expect_identical(object = x$region, expected = c("group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 1", "group 2", "group 2", "group 2", "group 2"))
      expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2", "unit 3", "unit 3", "unit 3", "unit 3"))
    } else {
      expect_identical(object = x$territories, expected = c("unit 1", "unit 1", "unit 1", "unit 1", "unit 2", "unit 2", "unit 2", "unit 2", "unit 3", "unit 3", "unit 3", "unit 3"))
    }
    expect_identical(object = x$year, expected = c("year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2", "year 1", "year 1", "year 2", "year 2"))
    expect_identical(object = x$commodities, expected = c("maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean", "maize", "soybean"))
    if(is.null(variables)){
      if(groups){
        expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 6)
        expect_names(x = colnames(x), permutation.of = c("region", "territories", "year", "commodities", "harvested", "production") )
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