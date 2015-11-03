#' @title SCOPUS Author Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' retrieval of an author from the Elsevier Author Retrieval API
#' @param id Identifier for author
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
#'    x = author_retrieval("40462056100", identifier = "author_id")
#' } else {
#'  x = author_retrieval(
#'  "40462056100",
#'  identifier = "author_id",
#'    api_key_error = FALSE)
#' }
author_retrieval <- function(
  id,
  identifier = c("author_id", "eid"),
  http_end = NULL,
  ...
){

  identifier = match.arg(identifier)
  ender = paste0("/", paste(identifier, id, sep = "/"))

  if (!is.null(http_end)) {
    ender = paste(ender, http_end, sep = "/")
  }
  s = generic_elsevier_api( type = "author",
                            http_end = ender, ...)
  return(s)

}

