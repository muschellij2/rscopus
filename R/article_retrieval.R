#' @title ScienceDirect Article Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' retrieval of an article from the Elsevier Article Retrieval API
#' @param id Identifier for article
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
#'     x = article_retrieval("S1053811915002700", identifier = "pii")
#' } else {
#'  x = article_retrieval("S1053811915002700",
#'    identifier = "pii",
#'    api_key_error = FALSE)
#' }
article_retrieval <- function(
  id, # Identifier for article
  identifier = c("scopus_id", "eid", "doi", "pii", "pubmed_id"), # type of identifier
  http_end = NULL, # any additional end to http statement.  See \code{\link{generic_elsevier_api}}
  ...
){

  identifier = match.arg(identifier)
  ender = paste0("/", paste(identifier, id, sep = "/"))

  if (!is.null(http_end)){
    ender = paste(ender, http_end, sep = "/")
  }

  s = generic_elsevier_api( type = "article",
                            http_end = ender, ...)
  return(s)
}

