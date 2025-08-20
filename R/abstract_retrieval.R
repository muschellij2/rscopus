#' @title SCOPUS Abstract Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' retrieval of an abstract from the Elsevier Abstract Retrieval API
#' @param id Identifier for abstract
#' @param identifier Type of identifier to use
#' @param http_end any additional end to http statement.
#' See \code{\link{generic_elsevier_api}}
#' @param view the amount of information to view, see
#' \url{https://dev.elsevier.com/sc_abstract_retrieval_views.html} for more info.
#' There is a mismatch between these docs and the API, so `"BASIC"` and
#' `"ENTITLED"` may have issues.
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' @export
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = abstract_retrieval("S1053811915002700", identifier = "pii",
#'    verbose = FALSE)
#'    x = abstract_retrieval("S1053811915002700", identifier = "pii",
#'    view = "FULL",
#'    verbose = FALSE)
#' }
abstract_retrieval <- function(
  id, # Identifier for abstract
  identifier = c("scopus_id", "eid", "doi", "pii", "pubmed_id", "pui", "group_id"), # type of identifier
  http_end = NULL, # any additional end to http statement.  See \code{\link{generic_elsevier_api}}
  view = c("META", "META_ABS", "FULL", "REF", "ENTITLED", "BASIC"),
  ...
){

  identifier = match.arg(identifier)
  id = gsub("SCOPUS_ID:", "", id, fixed = TRUE)
  id = gsub("DOI:", "", id, fixed = TRUE)
  ender = paste0("/", paste(identifier, id, sep = "/"))
  ender = gsub("//", "/", ender)
  view = match.arg(view)

  if (!is.null(http_end)) {
    ender = paste(ender, http_end, sep = "/")
  }
  ################################
  #
  ################################
#   arguments <- list(...)
#   n_args = names(arguments)
#   l_args = length(arguments)
#
#   if (length(n_args) != l_args){
#     warning("All arguments should be named in ..., may give unpredictable results")
#   }
#   if ("http_end" %in% n_args) {
#     ender = paste(http_end, ender, sep = "/")
#   }
  s = generic_elsevier_api( type = "abstract",
                            http_end = ender, ...)
  return(s)

}

