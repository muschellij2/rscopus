#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param affil_id Affiliation ID number
#' @param searcher Identifer for Affiliation ID.  Do not change unless you
#' know exactly what the API calls for.
#' @param ... Arguments to be passed to \code{\link{GET}}
#' @seealso \code{\link{get_author_info}}
#' @export
#' @return List of entries from SCOPUS
affil_search <- function(
  affil_id, # Author ID number
  searcher = "AF-ID",
  ...){
  all_entries = author_search(au_id = affil_id,
                              searcher = searcher, ...)
  return(all_entries)
}

