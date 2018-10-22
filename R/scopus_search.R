#' @title SCOPUS Search
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' scopus search from the Elsevier Scopus Search API
#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param query Query string to search on SCOPUS
#' @param api_key API Key for Elsevier
#' @param http Address for scopus API
#' @param count number of records to retrieve (below 200 for STANDARD,
#' below 25 for COMPLETE views, see
#' \url{https://dev.elsevier.com/api_key_settings.html}).
#' @param start where should the records start gathering
#' @param verbose Print diagnostic messages
#' @param max_count Maximum count of records to be returned.
#' @param view type of view to give, see
#' \url{https://api.elsevier.com/documentation/ScopusSearchAPI.wadl}
#' @param ... Arguments to be passed to the query list for
#' \code{\link{GET}}
#' @export
#' @return List of entries from SCOPUS
#' @examples
#' if (have_api_key()) {
#' res = scopus_search(query = "all(gene)", max_count = 20,
#' count = 10)
#' df = gen_entries_to_df(res$entries)
#' head(df$df)
#' sci_res = sciencedirect_search(query = "heart+attack AND text(liver)",
#' max_count = 30, count = 25)
#' sci_df = gen_entries_to_df(sci_res$entries)
#'
#' nt = sciencedirect_search(query = "title(neurotoxin)", max_count = 20,
#' count = 10)
#' nt_df = gen_entries_to_df(nt$entries)
#' nt_df = nt_df$df
#' }
scopus_search <- function(
  query, # Author ID number
  api_key = NULL,
  count = 200, # number of records to retrieve (below 25)
  view = c("STANDARD", "COMPLETE"),
  start = 0,
  verbose = TRUE,
  max_count = 20000,
  http = "https://api.elsevier.com/content/search/scopus",
  ...){

  api_key = get_api_key(api_key)
  view = match.arg(view)
  max_count_acceptable = switch(view,
                                STANDARD = 200,
                                COMPLETE = 25)

  if (count > max_count_acceptable) {
    warning("STANDARD view can have a max count of 200 and COMPLETE 25")
    count = max_count_acceptable
  }

  init_start = start
  # Wrapper to go through all the pages
  get_results = function(query, start = 0,
                         count = count,
                         verbose = TRUE, ...){
    q = list(
      query = query,
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
    L = list(get_statement = r, content = cr)
    return(L)
  }

  cr = get_results(query, start = init_start, count = count,
                   verbose = verbose,
                   ...)
  all_get = cr$get_statement
  cr = cr$content

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
                     "sent with current count"))
      pb = txtProgressBar(min = ifelse(n_runs == 2, 0, 1), max = n_runs - 1,
                          initial = 1, style = 3)
    }
    for (irun in seq(n_runs - 1)) {
      start = irun * count + init_start
      cr = get_results(query, start = start, count = count,
                       verbose = FALSE,
                       ...)
      all_get = c(all_get, cr$get_statement)
      cr = cr$content
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
  L = list(entries = all_entries, total_results = xtotal_results)
  L$get_statements = all_get
  L$facets = all_facets
  return(L)
}



#' @rdname scopus_search
#' @export
sciencedirect_search = function(
  count = 100,
  ...){
  count_choices = as.character(c(10, 25, 50, 100))
  count = as.character(count)
  count = match.arg(count, choices = count_choices)
  count = as.numeric(count)
  res <- scopus_search(
    count = count,
    ...,
    http = "https://api.elsevier.com/content/search/sciencedirect")
  return(res)
}

#' @rdname scopus_search
#' @export
scidir_search = sciencedirect_search
