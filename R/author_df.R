
#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param last_name last name of author
#' @param api_key Elsvier API key
#' @param first_name first name of author
#' @param verbose Print diagnostic messages
#' @param ... Arguments to be passed to \code{\link{author_search}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @return List of entries from SCOPUS
#' @import plyr
#' @examples \dontrun{
#' author_df(last_name = "Caffo", first_name = "Brian")
#' }
author_df = function(au_id, last_name,
                     first_name,
                     api_key = NULL,
                     verbose = TRUE,
                     ...){
  api_key = get_api_key(api_key)

  # Getting AU-ID
  if (
    (!missing(last_name) | !missing(first_name) ) &
    !missing(au_id)) {
    warning("AU-ID overriding first/last name combination")
  }
  if (missing(au_id)) {
    last_name = replace_non_ascii(last_name)
    first_name = replace_non_ascii(first_name)
    auth_name = get_author_info(last_name = last_name,
                                first_name = first_name,
                                api_key = api_key)
    if (NROW(auth_name) == 0) {
      stop("No author name found")
    }
    if (all(is.na(auth_name$auth_id))) {
      stop("No author name found")
    }
    if (verbose) {
      message("Authors found:")
      print(auth_name[1,])
    }
    au_id = auth_name$auth_id[1]
  }
  if (missing(last_name)) {
    last_name = NULL
  }
  if (missing(first_name)) {
    first_name = NULL
  }


  ### Getting author information
  entries = author_search(au_id = au_id, api_key = api_key,
                       verbose = verbose,
                       ...)$entries


  df = entries_to_df(entries = entries,
                     au_id = au_id,
                     verbose = verbose)


#   strip_info = lapply(info, function(x) {
#     x[c("dc:title",
#     "dc:creator", "prism:publicationName",
#     "prism:volume", "prism:issueIdentifier", "prism:pageRange", "prism:coverDate",
#     "prism:coverDisplayDate", "prism:doi", "citedby-count",
#     "affiliation", "prism:aggregationType", "subtype", "subtypeDescription",
#     "author-count", "author")]
#   })

  # affils = entries_to_affil_list(info)


  # df$n_affiliations = n_affils
  df$first_name = first_name
  df$last_name = last_name
  df$au_id = au_id
  # df = cbind(df, affils)
  # df = cbind(df, auths)
#   for (icol in grep("affil_", colnames(df))) {
#     df[, icol] = as.character(df[, icol])
#   }

  return(df)
}