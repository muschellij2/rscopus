#' @title Generic Elsevier Search
#'
#' @description Runs GET on generic Elsevier Search
#' @param query Query to run
#' @param type Type of search.  See \url{http://dev.elsevier.com/api_docs.html}
#' @param search_type Type of search if \code{type = "search"}.
#' See \url{http://dev.elsevier.com/api_docs.html}
#' @param api_key Elsvier API key
#' @param headers Headers passed to \code{\link{add_headers}},
#' passed to \code{\link{GET}}
#' @param content_type Is the data content or feedback?
#' @param root_http address to use for query
#' @param http_end string to add to end of http specification
#' (done using \code{paste0})
#' @param verbose Print messages from specification
#' @param api_key_error Should there be an error if no API key?
#' @param ... Options passed to query for \code{\link{GET}}
#' @return List of elements, content and the \code{GET} request
#' @import httr
#' @export
#' @examples \dontrun{
#' query_string = "affil(hopkins)"
#' # Use affiliation query
#' s = generic_elsevier_api(query = query_string,
#'                          type = "search", search_type = "affiliation",
#'                          api_key = api_key)
#'
#' # Use author query
#' s = generic_elsevier_api(query = query_string,
#' type = "search", search_type = "author",
#' api_key = api_key)
#'
#' # Query abstract by pii
#' s = generic_elsevier_api(query = "",
#'                       type = "abstract", http_end = "pii/S1053811915002700",
#'                       api_key = api_key)
#' }
generic_elsevier_api <- function(
  query = NULL,
  type = c("search", "article",
           "entitlement", "recommendation",
           "object", "fragment",
           "abstract", "affiliation",
           "embase", "author",
           "serial", "nonserial",
           "subject", "holdings",
           "citation-count", "citations"),
  search_type = c("affiliation", "author", "scopus",
           "scidir", "scidir-object"),
  api_key = NULL,
  headers = NULL,
  content_type = c("content", "feedback"),
  root_http = "http://api.elsevier.com",
  http_end = NULL,
  verbose = TRUE,
  api_key_error = TRUE,
  ...
  ){

  api_key = get_api_key(api_key, error = api_key_error)

  type = match.arg(type)
  content_type = match.arg(content_type)

  root_http = paste(root_http, content_type, sep = "/")

  search_type = switch(type,
    search = match.arg(search_type),
    embase = "article",
    serial = "title",
    nonserial = "title",
    entitlement = "entitlement",
    holdings = "report.url",
    "citation-count" = "citation-count",
    citations = "citations"
  )
  if (type %in% c("entitlement","recommendation")) {
    type = "article"
  }
  if (type %in% c("citation-count", "citations")) {
    type = "abstract"
  }

  http = paste(root_http, type, search_type, sep = "/")
  http = gsub("/$", "", http)
  http = paste0(http, http_end)

  if (verbose){
    message(paste0("HTTP specified is:", http, "\n"))
  }
  if (!is.null(api_key)){
    qlist = list(
      "apiKey" = api_key,
      query = query,
      ...)
  } else {
    qlist =  list(
      query = query, ...)
  }
  if (is.null(query)){
    qlist$query = NULL
  }
  if (length(qlist) > 0){
    r = GET(http,
            query = qlist,
            add_headers(headers)
    )
  } else {
    r = GET(http,
            add_headers(headers)
            )
  }
  cr = content(r)
  return(list(get_statement = r, content = cr))
}

