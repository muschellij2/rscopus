#' @title SCOPUS Author Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}}
#' to give a
#' retrieval of an author from the Elsevier Author Retrieval API
#' @param id Identifier for author
#' @param identifier Type of identifier to use
#' @param http_end any additional end to http statement.
#' See \code{\link{generic_elsevier_api}}
#'
#' @param ... Arguments to be passed to
#' \code{\link{generic_elsevier_api}}
#' @note See
#' \url{https://api.elsevier.com/documentation/AuthorRetrievalAPI.wadl}
#' for documentation
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to
#' \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = author_retrieval(au_id = "40462056100",
#'    verbose = FALSE)
#'    x = author_retrieval_id("40462056100", identifier = "author_id",
#'    verbose = FALSE)
#' } else {
#'  x = author_retrieval_id(
#'  "40462056100",
#'  identifier = "author_id",
#'    api_key_error = FALSE, verbose = FALSE)
#'  x = author_retrieval(
#'  au_id = "40462056100",
#'    api_key_error = FALSE, verbose = FALSE)
#' }
#' @rdname author_retrieval
#' @export
author_retrieval_id <- function(
  id,
  identifier = c("author_id", "eid"),
  http_end = NULL,
  ...
){

  identifier = match.arg(identifier)
  id = gsub("AUTHOR_ID:", "", id, fixed = TRUE)
  ender = paste0("/", paste(identifier, id, sep = "/"))

  if (!is.null(http_end)) {
    ender = paste(ender, http_end, sep = "/")
  }
  s = generic_elsevier_api(
    type = "author",
    http_end = ender, ...)
  return(s)

}

#' @rdname author_retrieval
#' @export
multi_author_retrieval <- function(
  id,
  identifier = c("author_id", "eid"),
  http_end = NULL,
  ...
){

  id = gsub("AUTHOR_ID:", "", id, fixed = TRUE)
  id = paste(id, collapse = ",")

  s = generic_elsevier_api(
    type = "author",
    http_end = http_end, ...)
  return(s)
}

#' @rdname author_retrieval
#' @param view Which view to see.  See
#' \url{https://api.elsevier.com/documentation/AuthorRetrievalAPI.wadl}
#' @param self_cite Should self-citations be included?
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param last_name last name of author
#' @param first_name first name of author
#' @param api_key Elsevier API key
#' @param verbose Print diagnostic messages
#' @param headers Headers passed to \code{\link{add_headers}},
#' passed to \code{\link{GET}}
#'
#' @export
author_retrieval <- function(
  au_id = NULL,
  last_name = NULL,
  first_name = NULL,
  view = c("LIGHT", "STANDARD",
           "ENHANCED", "METRICS", "ENTITLED"),
  self_cite = c("include", "exclude"),
  http_end = NULL,
  verbose = TRUE,
  api_key = NULL,
  headers = NULL,
  ...
){

  view = match.arg(view)
  self_cite = match.arg(self_cite)
  L = process_author_name(
    au_id = au_id,
    first_name = first_name,
    last_name = last_name,
    api_key = api_key,
    verbose = verbose,
    headers = headers)

  first_name = L$first_name
  last_name = L$last_name
  au_id = L$au_id

  s = author_retrieval_id(
    id = au_id,
    identifier = "author_id",
    http_end = http_end,
    api_key = api_key,
    view = view,
    `self-citation` = self_cite,
    verbose = verbose,
    headers = headers,
    ...)
  return(s)

}



