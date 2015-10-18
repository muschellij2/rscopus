#' @title SCOPUS Embase Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' retrieval of an Embase Entry from the Elsevier Embase Retrieval API
#' @param id Identifier for abstract
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
#'    x = embase_retrieval("S1053811915002700", identifier = "pii")
#' }
embase_retrieval <- function(
  id, # Identifier for embase
  identifier = c("lui", "pii", "doi", "embase", "pubmed_id", "medline"),
  http_end = NULL,
  ...
){

  identifier = match.arg(identifier)
  ender = paste0("/", paste(identifier, id, sep = "/"))

  if (!is.null(http_end)){
    ender = paste(ender, http_end, sep = "/")
  }

  s = generic_elsevier_api( type = "abstract",
                            http_end = ender, ...)
  return(s)

}

