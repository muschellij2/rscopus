#' SCOPUS Citation Retrieval
#'
#' @param scopus_id Scopus Identifier
#' @param pubmed_id Scopus Identifier
#' @param pii Scopus Identifier
#' @param doi Scopus Identifier
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' @export
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = citation_retrieval(pii = "S1053811915002700",
#'    verbose = FALSE)
#' }
citation_retrieval <- function(
  scopus_id = NULL,
  pii = NULL,
  doi = NULL,
  pubmed_id = NULL,
  ...
){


  query = list()
  if (!is.null(scopus_id)) {
    scopus_id = gsub("SCOPUS_ID:", "", scopus_id, fixed = TRUE)
  }
  query$scopus_id = scopus_id
  query$pii = pii
  if (!is.null(doi)) {
    doi = gsub("DOI:", "", doi, fixed = TRUE)
  }
  query$doi = doi
  query$pubmed_id = pubmed_id
  if (length(query) == 0) {
    query = NULL
  }

  s = generic_elsevier_api( type = "citations",
                            query = query)

  return(s)

}

