#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param affil_id Affiliation ID number
#' @param searcher Identifier for Affiliation ID.  Do not change unless you
#' know exactly what the API calls for.
#' @param http Address for scopus api
#' @param facets Facets sent in query.  See \url{https://dev.elsevier.com/api_docs.html}
#' @param ... Arguments to be passed to \code{\link{author_search}}
#' @seealso \code{\link{get_author_info}}
#' @param count number of records to retrieve (below 200, see
#' \url{https://dev.elsevier.com/api_key_settings.html})
#' @export
#' @return List of entries from SCOPUS
affil_search <- function(
  affil_id, # Author ID number
  searcher = "AF-ID",
  http = "https://api.elsevier.com/content/search/affiliation",
  facets = "affilcountry(sort=document-count)",
  count = 200,
  ...){

  affil_id = gsub("AFFILIATION_ID:", "", affil_id, fixed = TRUE)
  all_entries = author_search(au_id = affil_id,
                              searcher = searcher,
                              http = http,
                              count = count,
                              facets = facets, ...)
  return(all_entries)
}
