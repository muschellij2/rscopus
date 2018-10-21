#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Author ID number
#' @param api_key API Key for Elsevier
#' @param http Address for scopus api
#' @param count number of records to retrieve (below 200, see
#' \url{https://dev.elsevier.com/api_key_settings.html})
#' @param start where should the records start gathering
#' @param verbose Print diagnostic messages
#' @param facets Facets sent in query.  See
#' \url{https://dev.elsevier.com/api_docs.html}
#' @param searcher Identifier for author ID.  Do not change unless you
#' know exactly what the API calls for.
#' @param max_count Maximum count of records to be returned.
#' @param view type of view to give, see
#' \url{https://api.elsevier.com/documentation/AuthorSearchAPI.wadl}
#' @param add_query Things to add to the query parameter for the request
#' @param ... Arguments to be passed to the query list for
#' \code{\link{GET}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @importFrom httr stop_for_status parse_url build_url
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return List of entries from SCOPUS
#' @examples \dontrun{
#' author_search(au_id = "Smith", searcher = "affil(princeton) and authlast")
#' berk = author_search(au_id = "berkeley", searcher = "affil", count =100)
#' }
author_search <- function(
  au_id, # Author ID number
  api_key = NULL,
  http = "https://api.elsevier.com/content/search/author",
  count = 200, # number of records to retrieve (below 25)
  start = 0,
  verbose = TRUE,
  facets =  "subjarea(sort=fd,count=350)",
  searcher = "AU-ID",
  max_count = Inf,
  view = c("STANDARD", "COMPLETE"),
  add_query = NULL,
  ...){

  api_key = get_api_key(api_key)
  view = match.arg(view)
  max_count_acceptable = switch(
    view,
    STANDARD = 200,
    COMPLETE = 25)

  if (count > max_count_acceptable) {
    warning("STANDARD view can have a max count of 200 and COMPLETE 25")
    count = max_count_acceptable
  }

  init_start = start
  # Wrapper to go through all the pages
  get_results = function(au_id, start = 0,
                         count = count,
                         verbose = TRUE, ...){
    q = list(
      query = paste0(searcher, "(", au_id, ")", add_query),
      "APIKey" = api_key,
      count = count,
      start = start,
      view = view,
      ...)
    print_q = q
    print_q$APIKey = NULL
    if (verbose) {
      message("The query list is: ")
      print(dput(print_q))
    }
    r = GET(http,
            query = q,
            add_headers(
              "X-ELS-ResourceVersion" = "allexpand")
    )
    if (verbose) {
      parsed_url = httr::parse_url(r$url)
      parsed_url$query$APIKey = NULL
      r$url = httr::build_url(parsed_url)
      print(r)
    }
    stop_for_status(r)
    cr = content(r)$`search-results`
    return(cr)
  }


  au_id = as.character(au_id)
  au_id = gsub("AUTHOR_ID:", "", au_id, fixed = TRUE)
  au_id = gsub("AFFILIATION_ID:", "", au_id, fixed = TRUE)

  cr = get_results(au_id, start = init_start, count = count,
                   facets = facets,
                   verbose = verbose,
                   ...)

  all_facets = cr$facet
  # Find total counts
  total_results = as.numeric(cr$`opensearch:totalResults`)

  if (verbose) {
    message(paste0("Total Entries are ",
                   total_results,
            ifelse(init_start > 0, paste0(", but starting at ",
                   init_start), "")
            ))
  }
  xtotal_results = total_results
  total_results = total_results - init_start

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
      start = irun * count + init_start
      cr = get_results(au_id, start = start, count = count,
                       facets = facets,
                       verbose = FALSE,
                       ...)
      all_entries = c(all_entries, cr$entry)
      all_facets = c(all_facets, cr$facet)
      if (verbose) {
        # if ((irun %% 10) == 0) {
          # message(paste0("Run #", irun))
          setTxtProgressBar(pb, value = irun)
        # }
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
  return(list(entries = all_entries, facets = all_facets,
              total_results = xtotal_results))
}


#' @title Search Authors by Affiliation on SCOPUS
#'
#' @description Searches SCOPUS to get information about authors with
#' a certain affiliation
#' @param affil_id Affiliation ID number
#' @param searcher Identifier for Affiliation ID.  Do not change unless you
#' know exactly what the API calls for.
#' @param ... Arguments to be passed to \code{\link{GET}}
#' @seealso \code{\link{get_author_info}}
#' @export
#' @return List of entries from SCOPUS
author_search_by_affil <- function(
  affil_id, # Author ID number
  searcher = "AF-ID",
  ...){
  all_entries = author_search(au_id = affil_id,
                              searcher = searcher, ...)
  return(all_entries)
}
