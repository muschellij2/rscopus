
#' @title Search Affiliation Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an affiliation
#' @param affil_id Affiliation ID number.
#' @param verbose Print diagnostic messages
#' @param ... Arguments to be passed to \code{\link{author_search}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @return List of entries from SCOPUS
#' @import plyr
#' @note The \code{affil_data} command will return the list of all entries as well as
#' the \code{data.frame}
affil_df = function(
  affil_id,
  verbose = TRUE,
  ...){

  L = affil_data(
    affil_id = affil_id,
    verbose = verbose,
    ... = ...)
  df = L$df

  return(df)
}



#' @rdname affil_df
#' @export
affil_data = function(
  affil_id,
  verbose = TRUE,
  ...){

  entries = affil_search(
    affil_id = affil_id,
    verbose = verbose, ...)$entries

  df = entries_to_df(entries = entries,
                     au_id = NULL,
                     verbose = verbose)
  df$affid = affil_id

  L = list(entries = entries, df = df)
  return(L)
}