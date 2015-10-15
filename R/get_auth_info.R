
#' @title Get Author Information and ID from Scopus
#'
#' @description Uses SCOPUS author search to identify author identification
#' information for
#' @param last_name last name of author
#' @param first_name first name of author
#' @param api_key Elsvier API key
#' @param http Author API http
#' @import httr
#' @export
#' @return Matrix of data
get_author_info <- function(
                          last_name, # last name of author
                          first_name = NULL, # first name of author
                          api_key, # Elsvier API key
                          http = "http://api.elsevier.com/content/search/author" # Author API http
){
  if (missing(api_key)){
    api_key = getOption("elsevier_api_key")
  }
  if (is.null(api_key)){
    stop("API key not found")
  }
  query = ""
  if (!is.null(first_name)){
    query = paste0("AUTHFIRST(", first_name, ")+AND+")
  }
  query = paste0(query,
                 "AUTHLAST(", last_name, ")")

  url = paste0(http, "?query=", query,
               "&APIKey=", api_key)
  cr = content(GET(url))
  # xcr = cr
  if (!is.null(cr$`service-error`)){
    print(cr)
    stop("Service Error\n")
  }

  cr = cr$`search-results`$entry

  auth_get_info = function(cr){

    auth_names = cr$`name-variant`[[1]]
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

    c(auth_name = auth_names,
      auth_id = auth_id,
      affil_id = affil_id,
      affil_name = affil_name)
  }

  info = t(sapply(cr, auth_get_info))
  return(info)
}
