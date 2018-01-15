#' Get Links for next/first/last query
#'
#' @param result Object (list) with an element named \code{content}, usually
#' from \code{generic_elsevier_api}
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
next_url = function(result) {
  links = get_links(result)
  links = links[ links$ref %in% "next", ]
  links = links$href
  links
}

#' @rdname get_links
#' @export
#' @importFrom httr headers parse_url
get_next = function(result, ...) {
  url = result$get_statement$url
  query = httr::parse_url(url)$query
  http = next_url(result)
  r = httr::GET(http,
          query = query,
          httr::headers(result$get_statement),
          ...)
  cr = content(r)
  return(list(get_statement = r, content = cr))
}
