#' @title Get Affiliation Information and ID from Scopus
#'
#' @description Uses SCOPUS affiliation search to identify affiliation identification
#' information
#' @param affil_name name of affiliation
#' @param affil_id ID of affiliation
#' @param api_key Elsevier API key
#' @param verbose Print messages from specification
#' @export
#' @return A \code{data.frame} of affiliation information
get_affiliation_info <- function(
  affil_id = NULL,
  affil_name = NULL,
  api_key = NULL, verbose = FALSE) {

  run_search = function(id, searcher = "AF-ID") {
    res = affil_search(affil_id = id, searcher = searcher,
                       identifier = "affiliation_id", count = 25, max_count = 25,
                       verbose = verbose, api_key = api_key)
    res = gen_entries_to_df(res$entries)
    res$df
  }

  if (is.null(affil_id) & is.null(affil_name)) {
    stop("affiliation ID or name must be specified")
  }
    if (!is.null(affil_id) & !is.null(affil_name)) {
    warning("Affiliation ID will override affiliation name!")
    affil_name = NULL
  }

  if (!is.null(affil_id)) {
    affil_id = gsub("AFFILIATION_ID:", "", affil_id, fixed = TRUE)
    res = run_search(affil_id, searcher = "AF-ID")
  }
  if (!is.null(affil_name)) {
    res = run_search(affil_name, searcher = "affil")
  }

  res$affil_id = gsub("AFFILIATION_ID:", "", res$`dc:identifier`)
  res$affil_id  = trimws( res$affil_id )
  res$affil_name = res$"affiliation-name"
  return(res)
}