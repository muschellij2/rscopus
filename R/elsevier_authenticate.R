sc_authenticated = function(...) {
  res = elsevier_authenticate(...)
  return(res)
}

#' Authenticate API Key and get Token
#'
#' @param api_key Elsevier API key
#' @param api_key_error Should there be an error if no API key?
#' @param verbose Print messages from specification
#' @param choice Choice of which registered
#' See \url{https://dev.elsevier.com/tecdoc_api_authentication.html}
#' @param headers Headers passed to \code{\link[httr]{add_headers}},
#' passed to \code{\link[httr]{GET}}
#' @param ... Additional arguments to send to \code{\link[httr]{GET}}.
#'
#' @return List of content, the \code{GET} request,
#' and the token
#' @export
#'
#' @examples
#' if (have_api_key()) {
#'    auth = elsevier_authenticate()
#' }
elsevier_authenticate = function(
  api_key = NULL,
  api_key_error = TRUE,
  choice = NULL,
  verbose = TRUE,
  headers = NULL,
  ...) {

  api_key = get_api_key(api_key, error = api_key_error)
  content_type = "authenticate"
  http = "https://api.elsevier.com"
  http = paste(http, content_type, sep = "/")

  if (verbose) {
    parsed_url = httr::parse_url(http)
    parsed_url$query$APIKey = NULL
    parsed_url = httr::build_url(parsed_url)
    message(paste0("HTTP specified is: ", parsed_url, "\n"))
  }
  qlist = list()
  qlist$apiKey = api_key
  qlist$choice = choice
  hdrs = do.call(httr::add_headers, args = as.list(headers))

  r = httr::GET(http,
                query = qlist,
                hdrs,
                ...
  )
  httr::warn_for_status(r)
  cr = httr::content(r)
  L = list(get_statement = r, content = cr)
  token = cr$`authenticate-response`$authtoken
  if (!is.null(token)) {
    class(token) = "token"
  }
  L$token = token
  L$auth_type = cr$`authenticate-response`$`@type`
  return(L)
}

#' @export
#' @rdname elsevier_authenticate
is_elsevier_guest = function(...) {
  auth = elsevier_authenticate(...)
  toupper(auth$auth_type) == "GUEST"
}

#' @export
#' @rdname elsevier_authenticate
is_elsevier_authorized = function(...) {
  have_api_key() && !is_elsevier_guest(...)
}
