
#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param last_name last name of author
#' @param api_key Elsvier API key
#' @param first_name first name of author
#' @param verbose Print diagnostic messages
#' @param all_author_info Should all author info be recorded instead of that just to the
#' author given
#' @param ... Arguments to be passed to \code{\link{author_search}}
#' @export
#' @seealso \code{\link{get_author_info}}
#' @return List of entries from SCOPUS
#' @examples \dontrun{
#' author_df(last_name = "Muschelli", first_name = "John")
#' }
#' @note The \code{author_data} command will return the list of all entries as well as
#' the \code{data.frame}.
author_df = function(au_id, last_name,
                     first_name,
                     api_key = NULL,
                     verbose = TRUE,
                     all_author_info = FALSE,
                     ...){

  L = author_data(au_id = au_id,
                  last_name = last_name,
                  first_name = first_name,
                  api_key = api_key,
                  verbose = verbose,
                  all_author_info = all_author_info,
                  ... = ...)
  df = L$df

  return(df)
}




#' @rdname author_df
#' @export
author_data = function(au_id, last_name,
                     first_name,
                     api_key = NULL,
                     verbose = TRUE,
                     all_author_info = FALSE,
                     ...){

  api_key = get_api_key(api_key)

  L = process_author_name(au_id = au_id,
                          first_name = first_name,
                          last_name = last_name,
                          api_key = api_key,
                          verbose = verbose)

  first_name = L$first_name
  last_name = L$last_name
  au_id = L$au_id

  ### Getting author information
  entries = author_search(au_id = au_id,
                          api_key = api_key,
                          verbose = verbose,
                          ...)$entries



  if ( all_author_info ) {
    # df$indexer = seq(nrow(df))
    df = entries_to_df(entries = entries,
                        au_id = NULL,
                        verbose = verbose)
    # df = merge(df, df2, sort = FALSE, all.x = TRUE)
    # df = df[ order(df$indexer), ]
    # df$indexer = NULL
  } else {
    df = entries_to_df(entries = entries,
                       au_id = au_id,
                       verbose = verbose)
  }



  # df$n_affiliations = n_affils
  df$first_name = first_name
  df$last_name = last_name
  df$au_id = au_id
  L = list(entries = entries,
           df = df)

  return(L)
}




#' @title Process Author Name
#' @description Process author ID and names for generic use
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param last_name last name of author
#' @param first_name first name of author
#' @param api_key Elsvier API key
#' @param verbose Print diagnostic messages
#' @return List of first/last name and author ID
#' @note This function is really to avoid duplication
#' @export
process_author_name = function(au_id, last_name,
                               first_name, api_key = NULL, verbose = TRUE) {
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
    if (all(is.na(auth_name$au_id))) {
      stop("No author name found")
    }
    if (verbose) {
      message("Authors found:")
      print(auth_name[1,])
    }
    au_id = auth_name$au_id[1]
  }
  if (missing(last_name)) {
    last_name = NULL
  }
  if (missing(first_name)) {
    first_name = NULL
  }
  L = list(first_name = first_name,
           last_name = last_name,
           au_id = au_id)
  return(L)
}