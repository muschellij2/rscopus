#' Print method for token
#'
#' @return NULL
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods
#' @param reveal Should the token be revealed
#' @export
#'
#' @examples
#' x = "asdf"
#' class(x) = "token"
#' print(x)
#' print(x, reveal = TRUE)
#' @method print token
print.token = function(x, reveal = FALSE, ...) {
  if (reveal) {
    print(as.character(x), ...)
  } else {
    cat("<hidden token>")
  }
}

#' @export
print.ws_result = function(x, reveal = FALSE, ...) {
  if (reveal) {
    print(as.list(x), ...)
  } else {
    print(x$response)
  }
}

#' @export
#' @rdname print.token
reveal = function(x, ...) {
  print(x, reveal = TRUE, ...)
}
