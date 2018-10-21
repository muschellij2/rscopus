#' @title Get Complete Author Information and ID from Scopus
#'
#' @description Uses SCOPUS author search to identify author identification
#' information
#' @param last_name last name of author
#' @param first_name first name of author
#' @param affil_id ID of affiliation (optional)
#' @param api_key Elsevier API key
#' @param query Additional query info, added using \code{+AND+} to original query
#' @param http Author API http
#' @param verbose Print messages from specification
#' @param au_id Author ID number, will override first/last combination if
#' specified
#' @param affil_name name of affiliation
#' @param ... options to pass to \code{\link{GET}}
#' @importFrom httr GET content
#' @importFrom utils URLencode
#' @export
#' @return List of information
#' @examples
#' if (have_api_key()) {
#' res = get_complete_author_info(
#' last_name = "Muschelli",
#' first_name = "John",
#' verbose = FALSE)
#' }
get_complete_author_info <- function(
  last_name= NULL, # last name of author
  first_name = NULL, # first name of author
  affil_id = NULL,
  affil_name = NULL,
  api_key = NULL, # Elsevier API key
  http = "https://api.elsevier.com/content/search/author", # Author API http
  query = NULL,
  verbose = TRUE,
  au_id = NULL,
  ...
){
  api_key = get_api_key(api_key)

  reg_query = ""
  if (is.null(au_id)) {
    if (!is.null(first_name)) {
      reg_query = paste0("AUTHFIRST(", first_name, ")+AND+")
    }
    reg_query = paste0(reg_query,
                       "AUTHLAST(", last_name, ")")
    if (!is.null(query)) {
      reg_query = paste0(paste0(reg_query, collapse = "+AND+"), "+AND+", query)
    }
  } else {
    if (!is.null(last_name) || !is.null(first_name)) {
      warning("AU-ID will override this first/last name combo!")
    }
    au_id = gsub("AUTHOR_ID:", "", au_id, fixed = TRUE)
    reg_query = paste0("AU-ID(", au_id, ")")
  }

  if (!is.null(affil_id) | !is.null(affil_name)) {
    if (is.null(affil_id)) {
      res = get_affiliation_info(
        affil_id = affil_id,
        affil_name = affil_name)
      if (nrow(res) > 0) {
        res = res[1,]
        message(
          paste0("Using affiliation ID: ",
                 res$affil_id, " for ",
                 res$affil_name, ".  If incorrect, please ",
                 "specify affil_id directly or more specific",
                 " affiliation name ")
        )
      }
      affil_id = res$affil_id[1]
    }
    reg_query = paste0(reg_query, "+AND+", "AF-ID(", affil_id, ")")
  }

  reg_query = utils::URLencode(reg_query)
  # Need this way to not escape the `+` sign in the query
  url = paste0(http, "?query=", reg_query,
               "&APIKey=", api_key)
  if (verbose) {
    parsed_url = httr::parse_url(url)
    parsed_url$query$APIKey = NULL
    parsed_url = httr::build_url(parsed_url)
    message(paste0("HTTP specified is (without API key): ",
                   parsed_url, "\n"))
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


#' @title Get Relevant Author Information and ID from Scopus
#' in Data Frame
#'
#' @description Uses SCOPUS author search to identify author identification
#' information in a workable format
#' @param ... Arguments passed to \code{\link{get_complete_author_info}}
#' @seealso \code{\link{get_complete_author_info}}
#' @export
#' @return Data.frame of information
#' @examples \dontrun{
#' get_author_info(au_id = "40462056100")
#' }
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

    nonull2 = function(x){
      ifelse(is.null(x), "", x)
    }
    c(auth_name = nonull2(auth_names),
      au_id = nonull2(auth_id),
      affil_id = nonull2(affil_id),
      affil_name = nonull2(affil_name))
  }

  info = t(sapply(cr, auth_get_info))
  info = as.data.frame(info, stringsAsFactors = FALSE)
  return(info)
}
