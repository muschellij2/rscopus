#' Get Links for next/first/last query
#'
#' @param result Object (list) with an element named \code{content}, usually
#' from \code{generic_elsevier_api}
#' @param type The type of link requested
#' @param ... Options passed to \code{\link{GET}}
#'
#'
#' @return A \code{data.frame} or a vector of characters
#' @export
#'
#' @examples \dontrun{
#' result <- generic_elsevier_api(
#' query = "ISSN(0004-3702) AND YEAR(2001)",
#' search_type = "scopus")
#' next_result = get_next(result)
#'
#' }
#'
get_links = function(result) {
  result = result$content$`search-results`$link
  link_df = t(sapply(result, unlist))
  link_df = as.data.frame(link_df, stringsAsFactors = FALSE)
  cn = colnames(link_df)
  cn = gsub("@", "", cn)
  cn = gsub("^_", "", cn)
  colnames(link_df) = cn
  link_df
}

#' @rdname get_links
#' @export
get_url = function(
  result,
  type = c("self", "first", "prev", "next", "last")) {
  type = match.arg(type)
  links = get_links(result)
  links = links[ links$ref %in% type, ]
  links = links$href
  links
}

#' @rdname get_links
#' @export
next_url = function(result) {
  links = get_url(result, type = "next")
  links
}

#' @rdname get_links
#' @export
last_url = function(result) {
  links = get_url(result, type = "last")
  links
}

#' @rdname get_links
#' @export
prev_url = function(result) {
  links = get_url(result, type = "prev")
  links
}

#' @rdname get_links
#' @export
self_url = function(result) {
  links = get_url(result, type = "self")
  links
}

#' @rdname get_links
#' @export
first_url = function(result) {
  links = get_url(result, type = "first")
  links
}


#' @rdname get_links
#' @export
#' @importFrom httr headers parse_url
get_link_type = function(
  result, ...,
  type = c("self", "first", "prev", "next", "last")) {
  url = result$get_statement$url
  query = httr::parse_url(url)$query
  query$start = query$count = NULL
  http = get_url(result, type = type)
  r = httr::GET(
    http,
    query = query,
    httr::headers(result$get_statement),
    ...)
  cr = content(r)
  return(list(get_statement = r, content = cr))
}


#' @rdname get_links
#' @export
get_first = function(result, ...) {
  get_link_type(result, ..., type = "first")
}

#' @rdname get_links
#' @export
get_last = function(result, ...) {
  get_link_type(result, ..., type = "last")
}

#' @rdname get_links
#' @export
get_prev = function(result, ...) {
  get_link_type(result, ..., type = "prev")
}

#' @rdname get_links
#' @export
get_next = function(result, ...) {
  get_link_type(result, ..., type = "next")
}


#' @rdname get_links
#' @export
get_self = function(result, ...) {
  get_link_type(result, ..., type = "self")
}

