#' SCOPUS Citation Retrieval
#'
#' @param query Query to run, not overall query, but `queryParam` query
#' @param view Type of view to have.  See
#' \url{https://dev.elsevier.com/guides/ArticleMetadataViews.htm}
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' @export
#' @seealso \code{\link{generic_elsevier_api}},
#' \url{https://dev.elsevier.com/documentation/ArticleMetadataAPI.wadl}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = metadata_retrieval(query = "heart attack",
#'    verbose = FALSE)
#' }
metadata_retrieval <- function(
  query = NULL,
  view = c("STANDARD", "COMPLETE"),
  ...
){

  view = match.arg(view)
  s = generic_elsevier_api(
    type = "metadata",
    query = query,
    view = view,
    ...)

  return(s)

}

