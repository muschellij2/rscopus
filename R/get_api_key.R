#' @title Find API Key for Elsevier
#'
#' @description Determines if \code{option(elsevier_api_key)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#' @param api_key Elsvier API key
#' @return API key
get_api_key = function(api_key = NULL) {
  if (is.null(api_key)) {
    api_key = getOption("elsevier_api_key")
  }
  if (is.null(api_key)) {
    api_key = Sys.getenv("Elsevier_API", "")
    if (api_key %in% ""){
      api_key = NULL
    }
  }
  if (is.null(api_key)) {
    stop(paste0("API key not found, please set ",
                "option('elsevier_api_key') for general use or",
                "set environment variable Elsevier_API, to be",
                "accessed by Sys.getenv('Elsevier_API')"))
  }
  return(api_key)
}