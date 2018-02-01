
#' @title Search Affiliation Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an affiliation
#' @param affil_id Affiliation ID number.
#' @param affil_name affiliation name.  Disregarded if \code{affil_id} is
#' specified
#' @param verbose Print diagnostic messages
#' @param api_key Elsevier API key
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
  ...){

  L = affil_data(
    affil_id = affil_id,
    affil_name = affil_name,
    verbose = verbose,
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
    verbose = verbose, ...)$entries


  df = entries_to_df(entries = entries,
                     au_id = NULL,
                     verbose = verbose)
  df$affil_id = affil_id

  L = list(entries = entries, df = df)
  return(L)
}