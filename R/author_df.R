
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
  info = author_search(au_id = au_id, api_key = api_key,
                       verbose = verbose,
                       ...)$entries

  # Getting number of affilations to push out
  n_affils = sapply(info, function(x){
    length(x$affiliation)
  })

  ##################################
  # Get affiliation information
  ##################################
  # Need to make sure 1 for consistency in df
  max_n_affils = max(max(n_affils), 1)
  affils = t(sapply(info, function(x){
    x = sapply(x$affiliation, function(y){
      nonull(y$affilname, replace = "")
    })
    x = c(x, rep("",
                 max_n_affils - length(x)))
  }))

  # replace all missing with NA
  affils[affils %in% ""] = NA
  colnames(affils) = paste0("affil_",
                            seq(ncol(affils)))
  affils = as.data.frame(affils,
                         stringsAsFactors = FALSE)

  ### Get All possible affiliations from collaborators
  all_possible_affils = all_possible_affils(info)


#   strip_info = lapply(info, function(x) {
#     x[c("dc:title",
#     "dc:creator", "prism:publicationName",
#     "prism:volume", "prism:issueIdentifier", "prism:pageRange", "prism:coverDate",
#     "prism:coverDisplayDate", "prism:doi", "citedby-count",
#     "affiliation", "prism:aggregationType", "subtype", "subtypeDescription",
#     "author-count", "author")]
#   })

  auths = lapply(info, function(x){

    res = entry_to_affil(x = x,
                         all_affils = all_possible_affils)

    n_authors = max(as.numeric(res$seq))
    # print(n_authors)
    rres = res[ res$auth_id %in% au_id, , drop = FALSE]
    auth_order = unique(as.numeric(rres$seq))
    if (nrow(rres) == 0){
      auth_order = rep(NA, n_authors)
    }

    f_res = data.frame(
      cbind(n_auth = n_authors, auth_order = auth_order),
      stringsAsFactors = FALSE
    )
    rres = cbind(f_res, t(rres$affilname))
    rres = unique(rres)
    if (ncol(rres) > 2) {
      colnames(rres)[3:ncol(rres)] = paste0("affil_", 1:(ncol(rres) - 2) )
      # print(rres)
    }
    if (nrow(rres) == 0) {
      # print(res)
    }
    return(rres)
  })

  total_auths = max(sapply(auths, ncol))

  auths = lapply(auths, function(x) {
    if (ncol(x) < total_auths){
      mat = matrix(rep(NA, total_auths - ncol(x)), nrow = 1)
      colnames(mat) = paste0("affil_", ((ncol(x) + 1):total_auths) - 2)
      x = cbind(x, mat)
    }
    x
  })
  auths = do.call("rbind", auths)

  df$n_affiliations = n_affils
  df$first_name = first_name
  df$last_name = last_name
  # df = cbind(df, affils)
  df = cbind(df, auths)
  for (icol in grep("affil_", colnames(df))) {
    df[, icol] = as.character(df[, icol])
  }

  return(df)
}