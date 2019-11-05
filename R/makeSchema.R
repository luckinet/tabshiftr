#' Make a schema description
#'
#' @param schema [\code{list(2)}]\cr the list of schema information.
#' @importFrom checkmate assertList
#' @export

makeSchema <- function(schema = NULL){

  out <- new(Class = "schema",
             clusters = schema$clusters,
             variables = schema$variables)

  return(out)

}