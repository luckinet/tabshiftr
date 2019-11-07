#' Make a schema description
#'
#' @param schema [\code{list(2)}]\cr the list of schema information.
#' @importFrom checkmate assertList
#' @importFrom methods new
#' @export

makeSchema <- function(schema = NULL){

  out <- new(Class = "schema",
             clusters = schema$clusters,
             variables = schema$variables)

  return(out)

}