#' @title Find API Key for Elsevier
#'
#' @description Determines if \code{option(elsevier_api_key)} or
#' \code{option(elsevier_api_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#' @param api_key Elsvier API key
#' @param error Should the function error if \code{api_key = NULL}?
#' @note You can either set the API key using
#' \code{option(elsevier_api_key)} or have it accessible by
#' \code{api_key = Sys.getenv('Elsevier_API')}.
#' @return API key
#' @export
#' @examples
#' get_api_key(error = FALSE)
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
                  "had too many lines! Taking first \n"))
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
                "option('elsevier_api_key_filename') or",
                "option('elsevier_api_key') for general use or",
                "set environment variable Elsevier_API, to be",
                "accessed by Sys.getenv('Elsevier_API')"))
  }
  return(api_key)
}