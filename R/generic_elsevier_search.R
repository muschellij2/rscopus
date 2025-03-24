#' @title Generic Elsevier Search
#'
#' @description Runs GET on generic Elsevier Search
#' @param query Query to run, not overall query, but `queryParam` query
#' @param type Type of search.  See \url{https://dev.elsevier.com/api_docs.html}
#' @param search_type Type of search if \code{type = "search"}.
#' See \url{https://dev.elsevier.com/api_docs.html}
#' @param api_key Elsevier API key
#' @param headers Headers passed to \code{\link[httr]{add_headers}},
#' passed to \code{\link[httr]{GET}}
#' @param content_type Is the data content or feedback?
#' @param root_http address to use for query
#' @param http_end string to add to end of http specification
#' (done using \code{paste0})
#' @param verbose Print messages from specification
#' @param api_key_error Should there be an error if no API key?
#' @param ... Options passed to queryParam for \code{\link[httr]{GET}}
#' @return List of elements, content and the \code{GET} request
#' @export
#' @examples \dontrun{
#' query_string = "affil(hopkins)"
#' # Use affiliation query
#' s = generic_elsevier_api(query = query_string,
#'                          type = "search", search_type = "affiliation",
#'                          api_key = api_key,
#'                       verbose = FALSE)
#'
#' # Use author query
#' s = generic_elsevier_api(query = query_string,
#' type = "search", search_type = "author",
#' api_key = api_key,
#'                       verbose = FALSE)
#'
#' # Query abstract by pii
#' s = generic_elsevier_api(query = "",
#'                       type = "abstract", http_end = "pii/S1053811915002700",
#'                       api_key = api_key,
#'                       verbose = FALSE)
#' }
#' @importFrom httr GET add_headers
generic_elsevier_api <- function(
  query = NULL,
  type = c("search", "article",
           "entitlement", "recommendation",
           "object", "fragment",
           "abstract", "affiliation",
           "embase", "author",
           "serial", "nonserial",
           "subject", "holdings",
           "citation-count", "citations",
           "metadata", "ev",
           "ev_records", "analytics"),
  search_type = c("affiliation", "author", "scopus",
           "scidir", "scidir-object", "sciencedirect", "plumx"),
  api_key = NULL,
  headers = NULL,
  content_type = c("content", "feedback"),
  root_http = "https://api.elsevier.com",
  http_end = NULL,
  verbose = TRUE,
  api_key_error = TRUE,
  ...
  ){

  api_key = get_api_key(api_key, error = api_key_error)

  type = match.arg(type)
  if (!all(is.na(content_type))) {
    content_type = match.arg(content_type)
    root_http = paste(root_http, content_type, sep = "/")
  }

  search_type = switch(type,
    search = match.arg(search_type),
    analytics = match.arg(search_type),
    embase = "article",
    serial = "title",
    nonserial = "title",
    entitlement = "entitlement",
    holdings = "report.url",
    "citation-count" = "citation-count",
    citations = "citations",
    metadata = "article",
    ev = "results",
    ev_records = "records"
  )
  if (type %in% c("entitlement","recommendation")) {
    type = "article"
  }
  if (type %in% c("citation-count", "citations")) {
    type = "abstract"
  }
  if (type %in% c("ev_records")) {
    type = "ev"
  }


  http = paste(type, search_type, sep = "/")
  if (!is.null(http_end)) {
    http = paste0(http, http_end)
  }
  http = gsub("/$", "", http)
  http = gsub("//", "/", http)
  http = paste(root_http, http, sep = "/")

  if (verbose) {
    parsed_url = httr::parse_url(http)
    parsed_url$query$APIKey = NULL
    parsed_url = httr::build_url(parsed_url)
    message(paste0("HTTP specified is:", parsed_url, "\n"))
  }
  qlist = list(...)
  qlist$query = query
  qlist$apiKey = api_key
  hdrs = do.call(httr::add_headers, args = as.list(headers))
  if (length(qlist) > 0) {
    r = GET(http,
            query = qlist,
            hdrs
    )
  } else {
    r = GET(http,
            hdrs
    )
  }
  cr = content(r)
  return(list(get_statement = r, content = cr))
}

