#' @title Find API Key for Elsevier
#'
#' @description Determines if \code{option(elsevier_api_key)} or
#' \code{option(elsevier_api_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#' @param api_key Elsevier API key
#' @param error Should the function error if \code{api_key = NULL}?
#' @note You can either set the API key using
#' \code{option(elsevier_api_key)} or have it accessible by
#' \code{api_key = Sys.getenv('Elsevier_API')}.
#' @return API key
#' @export
#' @examples
#' res = get_api_key(error = FALSE)
get_api_key = function(api_key = NULL, error = TRUE) {
  if (is.null(api_key)) {
    api_key = getOption("elsevier_api_key")
  }
  if (is.null(api_key)) {
    api_key = Sys.getenv("Elsevier_API")
    if (api_key %in% ""){
      api_key = NULL
    }
  }

  if (is.null(api_key)) {
    api_key_filename = getOption("elsevier_api_key_filename")
    if (!is.null(api_key_filename)){
      api_key = readLines(api_key_filename)
    }
    if (length(api_key) > 1){
      warning(paste0("API key from ", getOption("elsevier_api_key_filename"),
                     " had too many lines! Taking first \n"))
      api_key = api_key[1]
    }
  }
  if (!is.null(api_key)){
    if (api_key %in% ""){
      api_key = NULL
    }
  }

  if (is.null(api_key) & error) {
    stop(paste0("API key not found, please set ",
                "option('elsevier_api_key_filename') or ",
                "option('elsevier_api_key') for general use or ",
                "set environment variable Elsevier_API, to be ",
                "accessed by Sys.getenv('Elsevier_API')"))
  }
  if (!is.null(api_key)) {
    class(api_key) = "scopus_api_key"
  }
  return(api_key)
}

#' @rdname get_api_key
#' @export
have_api_key = function(api_key = NULL) {
  api_key = get_api_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}

#' @title Set API Key for Elsevier
#'
#' @description Sets Elsevier API key using
#' if \code{option(elsevier_api_key)}
#' @param api_key Elsevier API key
#' @return NULL
#' @export
set_api_key = function(api_key) {
  options("elsevier_api_key" = api_key)
  invisible(NULL)
}

#' Add Institution or Authorization Token
#'
#' @param token Elsevier API token, usually from
#' \code{\link{elsevier_authenticate}}
#'
#' @return An object of class \code{token}, but really a character
#' @export
inst_token_header = function(token) {
  x = c("X-ELS-Insttoken" = token)
  class(x) = "token"
  x
}


#' @rdname inst_token_header
#' @export
auth_token_header = function(token) {
  x = c("X-ELS-Authtoken" = token)
  class(x) = "token"
  x
}

