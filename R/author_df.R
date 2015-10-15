
#' @title Search Author Content on SCOPUS
#'
#' @description Searches SCOPUS to get information about documents on an author.
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param last_name last name of author
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
    ( !missing(last_name) | !missing(first_name) ) &
      !missing(au_id)) {
    warning("AU-ID overriding first/last name combination")
  }
  if (missing(au_id)){
    auth_name = get_author_info(last_name = last_name,
                                first_name = first_name,
                                api_key = api_key)
    if (NROW(auth_name) == 0) {
      stop("No author name found")
    }
    if (verbose) {
      message("Authors found:")
      print(auth_name[1,])
    }
    au_id = auth_name$auth_id[1]
  }

  ### Getting author information
  info = author_search(au_id = au_id, api_key = api_key,
                       verbose = verbose,
                       ...)$entries

  # Getting number of affilations to push out
  n_affils = sapply(info, function(x){
    length(x$affiliation)
  })
  nonull = function(x, replace = NA){
    if (is.null(x) | length(x) == 0){
      x = replace
    }
    x
  }

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


  ##################################
  # Get Citation Information
  ##################################
  cites = sapply(info, function(x){
    x = as.numeric(nonull(x$`citedby-count`))
    x
  })

  ##################################
  # Get Journal Information
  ##################################
  journals = sapply(info, function(x){
    x = nonull(x$`prism:publicationName`)
    x
  })

  ##################################
  # Get Description of type of entry (article/presentation)
  ##################################
  desc = sapply(info, function(x){
    x = nonull(x$subtypeDescription)
    x
  })
  ##################################
  # Get Dates of Publication
  ##################################
  dates = t(sapply(info, function(x){
    cx = nonull(x$`prism:coverDate`)
    cdx = nonull(x$`prism:coverDisplayDate`)
    c(cover_date = cx,
      cover_display_date = cdx)
  }))

  ##################################
  # Get Titles of Publication
  ##################################
  titles = sapply(info, function(x){
    x = nonull(x$`dc:title`)
  })

  ##################################
  # Get pii number to grab pdf later
  ##################################
  sci_pii = sapply(info, function(x){
    x = nonull(x$pii)
  })

  ##################################
  # Put all into a data.frame
  ##################################
  df = data.frame(au_id = au_id,
                  citations = cites,
                  journal = journals,
                  description = desc,
                  title = titles,
                  pii = sci_pii,
                  n_affiliations = n_affils
  )
  df = cbind(df, dates)
  df = cbind(df, affils)
  return(df)
}