#' @title Get Complete Author Information and ID from Scopus
#'
#' @description Uses SCOPUS author search to identify author identification
#' information
#' @param last_name last name of author
#' @param first_name first name of author
#' @param api_key Elsvier API key
#' @param query Additional query info, added using \code{+AND+} to original query
#' @param http Author API http
#' @param verbose Print messages from specification
#' @param ... options to pass to \code{\link{GET}}
#' @import httr
#' @importFrom utils URLencode
#' @export
#' @return List of information
get_complete_author_info <- function(
  last_name, # last name of author
  first_name = NULL, # first name of author
  api_key = NULL, # Elsvier API key
  http = "http://api.elsevier.com/content/search/author", # Author API http
  query = NULL,
  verbose = TRUE,
  ...
){
  api_key = get_api_key(api_key)

  reg_query = ""
  if (!is.null(first_name)) {
    reg_query = paste0("AUTHFIRST(", first_name, ")+AND+")
  }
  reg_query = paste0(reg_query,
                 "AUTHLAST(", last_name, ")")
  if (!is.null(query)){
    reg_query = paste0(paste0(reg_query, collapse = "+AND+"), "+AND+", query)
  }

  reg_query = utils::URLencode(reg_query)
  # Need this way to not escape the `+` sign in the query
  url = paste0(http, "?query=", reg_query,
               "&APIKey=", api_key)
  if (verbose){
    message(paste0("HTTP specified is:", url, "\n"))
  }
  r = GET(url,
          add_headers(
            "X-ELS-ResourceVersion" = "allexpand"),
          ...)
  cr = content(r)
  # xcr = cr
  if (!is.null(cr$`service-error`)) {
    print(cr)
    stop("Service Error\n")
  }
  return(list(get_statement = r, content = cr))
}


#' @title Get Relevant Author Information and ID from Scopus in DataFrame
#'
#' @description Uses SCOPUS author search to identify author identification
#' information in a workable format
#' @param ... Arguments passed to \code{\link{get_complete_author_info}}
#' @import httr
#' @seealso \code{\link{get_complete_author_info}}
#' @export
#' @return Data.frame of information
get_author_info <- function(...){
  cr = get_complete_author_info(...)$content
  cr = cr$`search-results`$entry

  # Quick setup function
  auth_get_info = function(cr){

    auth_names = cr$`preferred-name`
    auth_names = paste(
      auth_names$`given-name`,
      auth_names$`surname`
    )
    auth_id = cr$`dc:identifier`
    auth_id = gsub("AUTHOR_ID:", "",
                   auth_id, fixed = TRUE)
    affil = cr$`affiliation-current`
    affil_name = affil$`affiliation-name`
    affil_id = affil$`affiliation-id`

    nonull = function(x){
      ifelse(is.null(x), "", x)
    }
    c(auth_name = nonull(auth_names),
      auth_id = nonull(auth_id),
      affil_id = nonull(affil_id),
      affil_name = nonull(affil_name))
  }

  info = t(sapply(cr, auth_get_info))
  info = as.data.frame(info, stringsAsFactors = FALSE)
  return(info)
}
