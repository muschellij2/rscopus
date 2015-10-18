#' @title SCOPUS Affiliation Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' retrieval of an affiliation from the Elsevier Affiliation Retrieval API
#' @param id Identifier for affiliation
#' @param identifier Type of identifier to use
#' @param http_end any additional end to http statement.
#' See \code{\link{generic_elsevier_api}}
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' @export
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = affiliation_retrieval("60006183", identifier = "affiliation_id")
#' }
affiliation_retrieval <- function(
  id,
  identifier = c("affiliation_id", "eid"),
  http_end = NULL, # any additional end to http statement.
  ...
){

  identifier = match.arg(identifier)
  ender = paste0("/", paste(identifier, id, sep = "/"))

  if (!is.null(http_end)) {
    ender = paste(ender, http_end, sep = "/")
  }
  s = generic_elsevier_api( type = "affiliation",
                            http_end = ender, ...)
  return(s)
}

