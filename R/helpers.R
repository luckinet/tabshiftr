#' Update the formating of a table
#'
#' This function updates the format of a table by applying a schema description
#' to it.
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param schema [\code{character(1)}]\cr the schema description of
#'   \code{input}.
#' @importFrom purrr map
#' @importFrom dplyr row_number arrange_at
#' @importFrom stringr str_remove_all str_extract_all coll
#' @importFrom tidyselect starts_with

.updateFormat <- function(input = NULL, schema = NULL){

  clusters <- schema@clusters
  variables <- schema@variables
  format <- schema@format

  if(!is.null(format$del)){
    if(format$del == "."){
      format$del <- "[.]"
    }
  }

  if(!is.null(format$dec)){
    if(format$dec == "."){
      format$dec <- "[.]"
    }
  }

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

    # capture flags
    if(length(format$flags$flag) != 0){
      theFlags <- map(seq_along(theVar), function(ix){
        temp <- str_extract_all(string = theVar[[ix]], pattern = coll(paste0(format$flags$flag, collapse = ""))) %>%
          unlist()
        if(length(temp) == 0){
          NA
        } else {
          temp
        }
      })
    }

    theVar <- map(seq_along(theVar), function(ix){
      tmp <- theVar[[ix]]

      if(length(tmp) != 0){
        # replace white-spaces
        tmp <- gsub(" |\xe2\x80\x80|\xe2\x80\x81|\xe2\x80\x82|\xe2\x80\x83|\xe2\x80\x84|\xe2\x80\x85|\xe2\x80\x86|\xe2\x80\x87|\xe2\x80\x88|\xe2\x80\x89|\xe2\x80\x8a|\xe2\x80\x8b|\xe2\x80\x8c|\xe2\x80\x8d|", "", tmp)

        # replace NA values
        if(!is.null(format$na)){
          tmp[tmp %in% format$na] <- NA
        }

        # replace thousands seperator
        if(!is.null(format$del)){
          tmp <- gsub(format$del, "", tmp)
        }

        # replace decimal seperator
        if(!is.null(format$dec)){
          tmp <- gsub(format$dec, ".", tmp)
        }

        # remove flags
        if(length(format$flags$flag) != 0){
          tmp <- str_remove_all(string = tmp, pattern = paste0("[", paste0(format$flags$flag, collapse = ""), "]"))
        }

        # multiply with factor
        tmp <- suppressWarnings(as.numeric(tmp)) * obsVars[[i]]$factor

        # apply function to aggregate duplicated values
        if(length(tmp) > 1){
          tmp <- sum(tmp, na.rm = TRUE)
        }
        return(tmp)
      } else {
        NA
      }

    })

    if(length(format$flags$flag) != 0){
      input <- input %>% bind_cols(tibble(!!paste0("flag_", names(obsVars)[i]) := unlist(theFlags)))
    }

    input[[which(names(obsVars)[i] == names(input))]] <- unlist(theVar)

  }

  if(length(format$flags$flag) != 0){
    input <- input %>%
      unite(col = "flag", starts_with("flag_"), sep = ", ", na.rm = TRUE)
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
#' @importFrom dplyr distinct select bind_cols if_any full_join
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
      wideColnames <- map(.x = seq_along(wideID), .f = function(jx){
        wideID[[jx]] %>%
          pivot_longer(cols = everything(),
                       names_to = "name",
                       values_to = names(wideID)[jx])
      })

      # sort the resulting list by the length of the tables, so that longer
      # tables are at the beginning of the following "join" sequence
      tempDims <- map_int(.x = seq_along(wideColnames), .f = function(jx){
        dim(wideColnames[[jx]])[[1]]
      })
      wideColnames <- wideColnames[order(tempDims, decreasing = TRUE)]

      if(!is.null(wideID)){
        wideColnames <- reduce(wideColnames, full_join, by = "name") %>%
          fill(everything())
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

        # remove columns that are both in equalID and tempObs (for example, when an id-variable is used as key)
        # dupEqualIDs <- which(!as.list(bind_cols(equalID)) %in% as.list(bind_cols(tempObs)))

        tempObs <- outObs
        if(!is.null(wideID)){
          names(tempObs$listed) <- c("key", wideNames)
          # newObs <- bind_cols(c(equalID[dupEqualIDs], tempObs), .name_repair = "minimal") %>%
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
          warning("rows(", paste0(dupObs$row, collapse = ", "), ") are summarised from several values.", call. = FALSE)
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
          if(!names(ids[ix]) %in% names(wideID)){
            if(tempDim[1] == dim(ids[[ix]])[1]){
              bla <- ids[ix]
              names(bla[[1]]) <- names(ids[ix])
              return(bla)
            } else if(all(dim(ids[[ix]]) == c(1, 1))){
              set_names(list(tibble(!!names(ids[ix]) := rep(ids[[ix]][[1]], tempDim[1]))), names(ids[ix]))
            }
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
        if(dims[1] != nrRows){
          temp <- tibble(X = rep(unlist(clust, use.names = FALSE), nrRows/dims[1]))
        } else {
          temp <- tibble(X = unlist(clust, use.names = FALSE))
        }
      }
      outClust <-  set_names(x = list(temp), nm = names(clust))

    }
  }

  return(c(outGrp, outClust, outIDs, outObs))

}

#' Evaluate .sum constructs
#'
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param groups [\code{list(3)}]\cr the groups-slot from a schema.
#' @param data [\code{integerish(.)}]\cr the cell column or row that should be
#'   adapted to groupings.
#' @return the position of the evaluated position
#' @importFrom dplyr mutate row_number if_else group_by summarise n ungroup
#'   left_join pull
#' @importFrom rlang eval_tidy

.eval_sum <- function(input = NULL, groups = NULL, data = NULL){

  out <- data

  if(!is.null(data)){

    rowGroups <- input %>%
      mutate(rn = as.double(row_number()),
             rn_new = rn)

    if(!is.null(groups$rows)){

      for(i in seq_along(groups$rows)){

        temp <- groups$rows[[i]]
        targetRows <- eval_tidy(temp$groups[[1]])

        rowGroups <- rowGroups %>%
          mutate(rn_new = if_else(rn_new %in% targetRows, min(targetRows), rn_new))

      }
      nrs <- rowGroups %>%
        group_by(rn_new) %>%
        summarise(nr = n()) %>%
        ungroup() %>%
        mutate(id = row_number())
      rowGroups$rn_new <- rep(nrs$id, nrs$nr)

      out <- rowGroups %>%
        left_join(tibble(rn = out), ., by = "rn") %>%
        pull(rn_new)

    }

    # if(!is.null(groups$cols)){
    #
    # }

  }

  return(out)

}

#' Evaluate .find constructs
#'
#' @param input [\code{character(1)}]\cr table to reorganise.
#' @param col [\code{list(2)}]\cr the output of the respective .find construct
#'   used to match in columns.
#' @param row [\code{list(2)}]\cr the output of the respective .find construct
#'   used to match in rows.
#' @return the columns or rows of the evaluated position
#' @importFrom checkmate assertNumeric assertList assertDataFrame
#' @importFrom rlang eval_tidy
#' @importFrom purrr map_int map_lgl
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr select mutate across pull if_else
#' @importFrom stringr str_count

.eval_find <- function(input = NULL, col = NULL, row = NULL){

  assertDataFrame(x = input)
  assertList(x = row, min.len = 1, null.ok = TRUE)
  assertList(x = col, min.len = 1, null.ok = TRUE)

  # in case to look for columns
  if(!is.null(col)){

    theCols <- NULL
    for(i in seq_along(col)){

      if(names(col)[i] == "position"){
        cols <-  as.numeric(1:dim(input)[2] %in% col[[i]])
      } else if(names(col)[i] == "find"){

        theCol <- col[[i]]
        term <- eval_tidy(theCol$by)

        if(is.function(term)){

          if(!is.null(theCol$row)){
            assertNumeric(x = theCol$row, len = 1, any.missing = FALSE)
            subset <- input[unique(theCol$row),]
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
            # message(ix)
            # str_count(string = paste(input[[ix]], collapse = " "), pattern = term)
            if(!is.null(theCol$row)){
              str_count(string = paste(input[[ix]][theCol$row], collapse = " "), pattern = term)
            } else {
              str_count(string = paste(input[[ix]], collapse = " "), pattern = term)
            }
          })
        }

        if(theCol$invert){
          temp <- cols
          temp[cols == 0] <- 1
          temp[cols != 0] <- 0
          cols <- temp
        }

      } else {
        next
      }

      theCols <- c(theCols, list(cols))

      if(i != 1){
        theCols <- reduce(theCols, col[[i - 1]])
      } else {
        if(length(col) == 1){
          theCols <- theCols[[1]]
        }
      }

    }

    out <- rep(seq_along(cols), cols)

  }

  # in case to look for rows
  if(!is.null(row)){

    theRows <- NULL
    for(i in seq_along(row)){

      if(names(row)[i] == "position"){
        rows <- input %>%
          mutate(it = if_else(row_number() %in% row[[i]], 1, 0)) %>%
          pull(it)
      } else if(names(row)[i] == "find"){

        theRow <- row[[i]]
        term <- eval_tidy(theRow$by)
        if(is.function(term)){

          if(!is.null(theRow$col)){
            assertNumeric(x = theRow$col, len = 1, any.missing = FALSE)
            subset <- input[,unique(theRow$col)]
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

          if(length(term) > 1){
            term <- paste0(term, collapse = "|")
          }

          if(!is.null(theRow$col)){
            rows <- input %>%
              mutate(it = if_else(if_any(theRow$col, ~ grepl(x = .x, pattern = term)), 1, 0)) %>%
              pull(it)
          } else {
            rows <- input %>%
              unite(col = all, everything(), sep = " ", na.rm = TRUE) %>%
              mutate(it = str_count(string = all, pattern = term)) %>%
              pull(it)
          }

        }

        if(theRow$invert){
          temp <- rows
          temp[rows == 0] <- 1
          temp[rows != 0] <- 0
          rows <- temp
        }

      } else {
        next
      }

      theRows <- c(theRows, list(rows))

      if(i != 1){
        theRows <- reduce(theRows, row[[i - 1]])
      } else {
        if(length(row) == 1){
          theRows <- theRows[[1]]
        }
      }

    }

    out <- rep(seq_along(theRows), theRows)

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
#' @param flags whether or not flags are in the test table.
#' @return Either an error message of the invalid expectations, or the output of
#'   the last successful expectation.
#' @importFrom testthat expect_identical
#' @importFrom checkmate expect_names expect_tibble expect_list assertChoice

.expect_valid_table <- function(x = NULL, units = 1, variables = NULL,
                                groups = FALSE, flags = FALSE){

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
        if(flags){
          expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 6)
          expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production", "flag") )
        } else {
          expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 5)
          expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
        }
      }
      expect_identical(object = x$harvested, expected = c(1121, 1111, 1221, 1211))
      expect_identical(object = x$production, expected = c(1122, 1112, 1222, 1212))
    } else {
      expect_tibble(x = x, any.missing = FALSE, nrows = 4, ncols = 4)
      expect_names(x = colnames(x), permutation.of = c("territories", "year", "commodities", variables) )
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
        if(flags){
          expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 6)
          expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production", "flag") )
        } else {
          expect_tibble(x = x, any.missing = FALSE, nrows = 8, ncols = 5)
          expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
        }
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
        if(flags){
          expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 6)
          expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production", "flag") )
        } else {
          expect_tibble(x = x, any.missing = FALSE, nrows = 12, ncols = 5)
          expect_names(x = colnames(x), permutation.of =c("territories", "year", "commodities", "harvested", "production") )
        }
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