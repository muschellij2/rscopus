
#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Author ID number
#' @param api_key API Key for Elsevier
#' @param http Address for scopus api
#' @param count number of records to retrieve (below 100)
#' @param verbose Print diagnostic messages
#' @param ... Arguments to be passed to \code{\link{GET}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @return List of entries from SCOPUS
author_search <- function(
  au_id, # Author ID number
  api_key,
  http = "http://api.elsevier.com/content/search/scopus",
  count = 100, # number of records to retrieve (below 100)
  verbose = TRUE,
  ...){

  if (missing(api_key)){
    api_key = getOption("elsevier_api_key")
  }
  if (is.null(api_key)){
    stop("API key not found")
  }

  get_results = function(au_id, start = 0, count = count, ...){
    r = GET(http,
            query = list(
              query = paste0("AU-ID(", au_id, ")"),
              "APIKey" = api_key,
              count = count,
              start = start,
              ...)
    )
    cr = content(r)$`search-results`
    return(cr)
  }

  cr = get_results(au_id, start = 0, count = count)
  total_results = as.numeric(cr$`opensearch:totalResults`)
#   start_index = as.numeric(cr$`opensearch:startIndex`)
#   items_per_page = as.numeric(cr$`opensearch:itemsPerPage`)

  if (verbose){
    message(paste0("Total Entries are ",
      total_results, "\n"))
  }
  all_entries = cr$entry
  n_runs = ceiling(total_results / count)
  if (n_runs > 1){
    for (irun in seq(n_runs - 1)){
      start = irun * count
      cr = get_results(au_id, start = start, count = count)
      all_entries = c(all_entries, cr$entry)
    }
  }
  if (verbose){
    message(paste0("Number of Output Entries are ", length(all_entries),
                 "\n"))
  }
  if (total_results != length(all_entries)){
    warning("May not have received all entries")
  }
  return(all_entries)
}

