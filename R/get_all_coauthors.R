
#' Get all Co-Authors
#'
#' @param ... arguments to pass to \code{\link{author_df}}
#'
#' @return A list of output, incluing a vector of author_ids
#' @export
#'
#' @examples
#'  \dontrun{
#'  get_all_coauthors(last_name = "muschelli")
#'  }
get_all_coauthors = function(...) {
  #######################################################
  # Find an author information
  #######################################################
  res = author_df(...)
  # mydata = get_complete_author_info(last_name = "Muschelli")
  scopus_ids = sub("SCOPUS_ID:", "", res$`dc:identifier`)

  scopus_id = scopus_ids[1]
  get_list = function(scopus_id, verbose = TRUE) {
    out = rscopus::abstract_retrieval(scopus_id, verbose = verbose)
    author_df = jsonlite::fromJSON(
      jsonlite::toJSON(out$content$`abstracts-retrieval-response`$authors),
                         flatten = TRUE)$author
    list(
      full_output = out,
      author_df = author_df
    )
  }
  if (requireNamespace("pbapply", quietly = TRUE)) {
    output = pbapply::pblapply(scopus_ids, get_list, verbose = FALSE)
  } else {
    output = lapply(scopus_ids, get_list, verbose = TRUE)
  }


  names(output) = scopus_ids

  all_authors = lapply(output, function(x) x$author_df)
  all_authors = dplyr::bind_rows(all_authors, .id = "scopus_id")
  all_authors$au_id = sapply(all_authors$`@auid`, function(x)  {
    stopifnot(length(x) ==1)
    c(x)
  })

  collaborators = sort(unique(all_authors$au_id))
  out = list(
    collaborator_au_ids = collaborators,
    author_data = res,
    all_output = output,
    author_data = all_authors
  )
}

