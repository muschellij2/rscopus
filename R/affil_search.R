#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Affiliation ID number
#' @param api_key API Key for Elsevier
#' @param http Address for scopus api
#' @param count number of records to retrieve (below 100)
#' @param verbose Print diagnostic messages
#' @param ... Arguments to be passed to \code{\link{GET}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @return List of entries from SCOPUS
affil_search <- function(
  affil_id, # Author ID number
  api_key = NULL,
  http = "http://api.elsevier.com/content/search/scopus",
  count = 100, # number of records to retrieve (below 100)
  verbose = TRUE,
  ...){

  x = author_search(au_id = affil_id, searcher = "AF-ID", ...)

  return(all_entries)
}

