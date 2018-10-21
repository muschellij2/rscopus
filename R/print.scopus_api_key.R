#' Print method for Scopus API key
#'
#' @return NULL
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods
#' @param reveal Should the API key be revealed
#' @export
#'
#' @examples
#' x = "asdf"
#' class(x) = "scopus_api_key"
#' print(x)
#' print(x, reveal = TRUE)
#' @method print scopus_api_key
print.scopus_api_key = function(x, reveal = FALSE, ...) {
  if (reveal) {
    print(as.character(x), ...)
  } else {
    cat("<hidden api key, use print(, reveal = TRUE) to see it>")
  }
}
