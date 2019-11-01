#' Set new names to the output table
#' @param temp a temporary table on which to set names.
#' @param meta the schema description that is the basis to derive the names.
#' @importFrom checkmate assertNames
#' @export

setNames <- function(temp = NULL, meta = NULL){

  assertDataFrame(x = temp)
  assertList(x = meta, len = 3)
  assertNames(x = names(meta), permutation.of = c("cluster", "var_type", "table"))

  if(is.null(meta$table$gather_into) & is.null(meta$table$spread_from)){
    return(temp)
  } else if(!is.null(meta$var_type$key)){
    colnames(temp) <- c(meta$var_type$ids, meta$var_type$vals[meta$var_type$key])
  } else {
    colnames(temp) <- c(meta$var_type$ids, meta$var_type$vals)
  }

  return(temp)
}