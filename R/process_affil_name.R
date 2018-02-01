#' @title Process Affiliation Name
#' @description Process Affiliation ID and names for generic use
#' @param affil_id Affiliation ID number.
#' @param affil_name affiliation name
#' @param api_key Elsevier API key
#' @param verbose Print diagnostic messages
#' @return List of affiliation name and affiliation ID
#' @export
process_affiliation_name = function(
  affil_id = NULL,
  affil_name = NULL,
  api_key = NULL, verbose = TRUE) {
  # Getting AU-ID
  if (is.null(affil_id) && is.null(affil_name)) {
    warning("Affiliation name and ID are not specified")
  }
  if (is.null(affil_name)) {
    res = affiliation_retrieval(
      id = affil_id,
      identifier = "affiliation_id",
      verbose = verbose, api_key = api_key)
    affil_name = res$content$`affiliation-retrieval-response`$"affiliation-name"
    if (NROW(affil_name) == 0) {
      stop("No affiliation name found")
    }
    if (verbose) {
      message("Affilations found:")
      print(affil_name)
    }
  }
  if (is.null(affil_id)) {
    res = affil_search(
      affil_id = affil_name,
      searcher = "affil",
      identifier = "affiliation_id",
      count = 25,
      max_count = 25,
      verbose = verbose,
      api_key = api_key)
    affil_id = sapply(res$entries, `[[`, "dc:identifier")
    if (length(affil_name) == 0) {
      stop("No affiliation ids found")
    }
    affil_id = affil_id[[1]]
    affil_id = sub(".*:", "", affil_id)
    aff_names = sapply(res$entries, `[[`, "affiliation-name")
    affil_name = aff_names[[1]]
    if (verbose) {
      message("Affiliation IDs found:")
      print(affil_id)
    }
  }
  affil_id = as.character(affil_id)
  L = list(affil_name = affil_name,
           affil_id = affil_id)
  return(L)
}