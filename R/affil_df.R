
#' @title Search Affiliation Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an affiliation
#' @param affil_id Affiliation ID number.
#' @param affil_name affiliation name.  Disregarded if \code{affil_id} is
#' specified
#' @param verbose Print diagnostic messages
#' @param api_key Elsevier API key
#' @param facets Facets sent in query.  See \url{https://dev.elsevier.com/api_docs.html}
#' @param sort sorting sent to query
#' @param ... Arguments to be passed to \code{\link{author_search}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @return List of entries from SCOPUS
#' @note The \code{affil_data} command will return the list of all entries as well as
#' the \code{data.frame}
affil_df = function(
  affil_id = NULL,
  affil_name = NULL,
  api_key = NULL,
  verbose = TRUE,
  facets =  NULL,
  sort = "document-count",
  ...){

  L = affil_data(
    affil_id = affil_id,
    affil_name = affil_name,
    verbose = verbose,
    facets = facets,
    sort = sort,
    ... = ...)
  df = L$df

  return(df)
}



#' @rdname affil_df
#' @export
affil_data = function(
  affil_id = NULL,
  affil_name = NULL,
  api_key = NULL,
  verbose = TRUE,
  facets =  NULL,
  sort = "document-count",
  ...){

  #   entries = affil_search(
  #     affil_id = affil_id,
  #     verbose = verbose, ...)$entries
  if (is.null(affil_id)) {
    res = process_affiliation_name(
      affil_id = affil_id,
      affil_name = affil_name,
      api_key = api_key, verbose = verbose
    )
    affil_id = res$affil_id
  }
  entries = author_search_by_affil(
    affil_id = affil_id,
    verbose = verbose,
    facets = facets,
    sort = sort,
    ...)
  total_results = entries$total_results
  facets = entries$facets
  entries = entries$entries

  df = gen_entries_to_df(
    entries = entries)
  df$df$affil_id = affil_id

  L = list(entries = entries, df = df)
  L$total_results = total_results
  L$facets = facets
  return(L)
}