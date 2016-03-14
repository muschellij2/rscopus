
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

#   au_id = NULL
#   last_name = NULL
#   first_name = NULL

  entries = affil_search(
    affil_id = affil_id,
    verbose = verbose, ...)$entries

  df = entries_to_df(entries = entries,
                     au_id = NULL,
                     verbose = verbose)
  df$affil_id = affil_id

  return(df)
}



#' @rdname affil_df
affil_data = function(
  affil_id,
  verbose = TRUE,
  ...){

  #   au_id = NULL
  #   last_name = NULL
  #   first_name = NULL

  entries = affil_search(
    affil_id = affil_id,
    verbose = verbose, ...)$entries

  df = entries_to_df(entries = entries,
                     au_id = NULL,
                     verbose = verbose)
  df$affil_id = affil_id

  L = list(entries = entries, df = df)
  return(L)
}