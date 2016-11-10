#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Author ID number
#' @param api_key API Key for Elsevier
#' @param http Address for scopus api
#' @param count number of records to retrieve (below 25, see
#' \url{http://dev.elsevier.com/api_key_settings.html})
#' @param verbose Print diagnostic messages
#' @param facets Facets sent in query.  See \url{http://dev.elsevier.com/api_docs.html}
#' @param searcher Identifer for author ID.  Do not change unless you
#' know exactly what the API calls for.
#' @param max_count Maximum count of records to be returned.
#' @param ... Arguments to be passed to \code{\link{GET}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @importFrom httr stop_for_status
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return List of entries from SCOPUS
author_search <- function(
  au_id, # Author ID number
  api_key = NULL,
  http = "http://api.elsevier.com/content/search/scopus",
  count = 25, # number of records to retrieve (below 25)
  verbose = TRUE,
  facets =  "subjarea(sort=fd)",
  searcher = "AU-ID",
  max_count = Inf,
  ...){

  api_key = get_api_key(api_key)

  # Wrapper to go through all the pages
  get_results = function(au_id, start = 0, count = count, ...){
    r = GET(http,
            query = list(
              query = paste0(searcher, "(", au_id, ")"),
              "APIKey" = api_key,
              count = count,
              start = start,
              view = "COMPLETE",
              ...),
            add_headers(
              "X-ELS-ResourceVersion" = "allexpand")
    )
    stop_for_status(r)
    cr = content(r)$`search-results`
    return(cr)
  }

  cr = get_results(au_id, start = 0, count = count,
                   facets = facets)
  all_facets = cr$facet
  # Find total counts
  total_results = as.numeric(cr$`opensearch:totalResults`)


  if (verbose) {
    message(paste0("Total Entries are ",
                   total_results))
  }
  if (total_results > max_count) {
    total_results = max_count
    if (verbose) {
      message(paste0("Maximum Count is ",
                     total_results))
    }
  }
#   start_index = as.numeric(cr$`opensearch:startIndex`)
#   items_per_page = as.numeric(cr$`opensearch:itemsPerPage`)


  ### Loop through all the other pages
  all_entries = cr$entry
  n_runs = ceiling(total_results / count)
  if (n_runs > 1) {
    if (verbose) {
      message(paste0(n_runs, " runs need to be ",
                     "sent with curent count"))
      pb = txtProgressBar(min = ifelse(n_runs == 2, 0, 1), max = n_runs - 1,
                          initial = 1, style = 3)
    }
    for (irun in seq(n_runs - 1)) {
      start = irun * count
      cr = get_results(au_id, start = start, count = count,
                       facets = facets)
      all_entries = c(all_entries, cr$entry)
      all_facets = c(all_facets, cr$facet)
      if (verbose) {
        if ((irun %% 10) == 0) {
          # message(paste0("Run #", irun))
          setTxtProgressBar(pb, value = irun)
        }
      }
    }
   if (verbose) {
     close(pb)
   }
  }
  if (verbose) {
    message(paste0("Number of Output Entries are ", length(all_entries),
                 "\n"))
  }
  if (total_results != length(all_entries)) {
    warning("May not have received all entries")
  }
  return(list(entries = all_entries, facets = all_facets))
}

