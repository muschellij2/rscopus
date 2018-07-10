create_chunks = function(x, chunk_size = 25) {
  n = length(x)
  cuts <- ceiling(n/chunk_size)
  y <- rep(1:cuts, each = chunk_size)
  y = y[1:n]
  return(y)
}

#' Get Complete Author Information and ID from Scopus
#'
#' @param au_id vector of Author IDs
#' @param api_key Elsevier API key
#' @param verbose Print messages from specification
#' @param ... options to pass to \code{\link{generic_elsevier_api}}
#' @export
#' @return List of information
complete_multi_author_info <- function(
  au_id = NULL,
  api_key = NULL, # Elsevier API key
  verbose = TRUE,
  ...
){

  au_id = unique(au_id)
  au_id = trimws(au_id)
  au_id = unique(au_id)
  au_id = au_id[ !au_id %in% "" ]
  pasted_au_id = paste(au_id, collapse = ",")

  n = length(au_id)
  run_res = function(au_id, verbose, api_key, ...) {
    au_id = paste(au_id, collapse = ",")
    res = generic_elsevier_api(
      author_id = au_id,
      verbose = verbose,
      type = "author",
      api_key = api_key,
      ...)
    return(res)
  }
  chunk_size = 25
  y = create_chunks(au_id, chunk_size = chunk_size)
  uy = unique(y)
  if (n > chunk_size) {
    if (verbose) {
      message(paste0(
        "Over ", chunk_size,
        " authors requested, chunking into ",
        max(uy), " chunks")
      )
    }
  }

  all_res = vector(mode = "list",
                   length = 2)
  names(all_res) = c("get_statement", "content")

  for (iy in uy) {
    keep = y == iy
    if (n > chunk_size) {
      if (verbose) {
        message(paste0("Running chunk ", iy))
      }
    }
    res = run_res(au_id = au_id[keep],
                        verbose = verbose,
                        api_key = api_key,
                        ... = ...)
    all_res$get_statement = c(all_res$get_statement, list(res$get_statement))
    all_res$content =
      c(all_res$content,
        res$content$`author-retrieval-response-list`$`author-retrieval-response`)
  }
  all_res$au_id = pasted_au_id

  return(all_res)
}


#' Get Relevant Authors Information from IDs from Scopus
#'
#' @param ... Arguments passed to \code{\link{get_complete_author_info}}
#' @seealso \code{\link{get_complete_author_info}}
#' @return Data.frame of information
#' @examples \dontrun{
#' multi_author_info(au_id =  c("22968535800", "40462056100"))
#' }
#' @export
multi_author_info <- function(...){
  res = complete_multi_author_info(...)
  res = process_complete_multi_author_info(res)
  return(res)
}

#' @export
#' @param res result from \code{\link{complete_multi_author_info}}
#' @rdname multi_author_info
process_complete_multi_author_info = function(res) {
  cr = res$content
  au_id = res$au_id
  au_id = strsplit(au_id, split = ",")[[1]]
  au_id = au_id[ !au_id %in% "" ]

  no_at = function(y) {
    cn = names(y)
    cn = gsub("@", "", cn, fixed = TRUE)
    names(y) = cn
    y
  }

  no_at_colnames = function(y) {
    cn = colnames(y)
    cn = gsub("@", "", cn, fixed = TRUE)
    colnames(y) = cn
    y
  }

  get_core = function(x) {
    cd = x$coredata
    cd$link = NULL
    cd = unlist(cd)
    return(cd)
  }

  make_affil = function(affil) {
    affil = unlist(affil)
    # need this for duplication
    names(affil) = sub("@country", "_country", names(affil),
                       fixed = TRUE)
    affil = no_at(affil)
    cn = names(affil)
    cn = sub("\\$$", "", cn)
    cn = sub("[.]$", "", cn)
    names(affil) = cn
    affil
  }
  curr_affil = function(x) {
    affil = x$`author-profile`$`affiliation-current`$affiliation
    if (is.null(affil)) {
      return(NULL)
    }
    make_affil(affil)
  }

  affil_hist = function(x) {
    affil = sapply(x$`author-profile`$`affiliation-history`$affiliation,
                   make_affil)
    if (length(affil) == 0) {
      return(NULL)
    }
    keep = c("affiliation-id", "parent", "source", "ip-doc.id", "ip-doc.type",
             "ip-doc.relationship", "ip-doc.afdispname",
             "ip-doc.preferred-name.source",
             "ip-doc.preferred-name", "ip-doc.parent-preferred-name.source",
             "ip-doc.parent-preferred-name", "ip-doc.sort-name",
             "ip-doc.address.country",
             "ip-doc.address.address-part", "ip-doc.address.city",
             "ip-doc.address.state",
             "ip-doc.address.postal-code", "ip-doc.org-domain",
             "ip-doc.org-URL",
             "ip-doc.afnameid", "ip-doc.manual-curation.curated",
             "ip-doc.manual-curation.date-curation.day",
             "ip-doc.manual-curation.date-curation.month",
             "ip-doc.manual-curation.date-curation.timestamp",
             "ip-doc.manual-curation.date-curation.year",
             "ip-doc.manual-curation.curation-source",
             "ip-doc.manual-curation.curation-type")
    affil = t(sapply(affil, function(r) {
      r = r[keep]
      names(r) = keep
      r
    }))
    affil = as.data.frame(affil, stringsAsFactors = FALSE)
    return(affil)
  }

  # sapply(cr, function(x) sapply(x$`author-profile`$`affiliation-history`$affiliation, unlist))

  subjareas = function(x) {
    sa = x$`subject-areas`$`subject-area`
    if (is.null(sa)) {
      return(NULL)
    }
    sa = bind_rows(sa)
    cn = colnames(sa)
    cn = setdiff(cn, "@_fa")
    sa = sa[, cn]
    cn = gsub("@", "", cn, fixed = TRUE)
    cn[ cn == "$"] = "longname"
    colnames(sa) = cn
    return(sa)
  }

  classes = function(x) {
    class_freq = x$`author-profile`$classificationgroup
    class_freq = class_freq$classifications$classification
    if (is.null(class_freq)) {
      return(NULL)
    }
    class_freq = t(sapply(class_freq, unlist))
    cn = colnames(class_freq)
    cn = gsub("@", "", cn, fixed = TRUE)
    cn[ cn == "$"] = "code"
    colnames(class_freq) = cn
    class_freq = as.data.frame(class_freq, stringsAsFactors = FALSE)
    class_freq
  }

  run_years = function(x) {
    y = x$`author-profile`$`publication-range`
    if (is.null(y)) {
      return(NULL)
    }
    y = unlist(y)
    y = no_at(y)
    y
  }

  pref_name = function(x) {
    y = x$`author-profile`$`preferred-name`
    if (is.null(y)) {
      return(NULL)
    }
    y = unlist(y)
    cn = names(y)
    cn = gsub("@", "", cn, fixed = TRUE)
    cn = setdiff(cn, c("date-locked", "source"))
    names(y) = cn
    y = y[cn]
    y
  }


  other_names = function(x) {
    r = x$`author-profile`$`name-variant`
    if (is.null(r)) {
      return(NULL)
    }
    if (!is.null(names(r))) {
      r = list(r)
    }
    r = lapply(r, nonull)
    r = lapply(r, function(xx) {
      lapply(xx, nonull)
    })
    r = bind_rows(r)
    r = no_at_colnames(r)
    # r = sapply(r, function(y) {
    #   unlist(y)
    #   y = no_at(y)
    #   keep = c("doc-count", "initials",
    #            "indexed-name", "surname",
    #            "given-name")
    #   y = y[keep]
    #   names(y) = keep
    #   y
    # })
    # r = t(r)
    return(r)
  }


  journals = function(x) {
    j = x$`author-profile`$`journal-history`$journal
    j = lapply(j, unlist)
    j = t(sapply(j, function(y) {
      keep = c("type", "sourcetitle", "sourcetitle-abbrev", "issn")
      y = no_at(y)
      y = y[keep]
      names(y) = keep
      y
    }))
    j = as.data.frame(j, stringsAsFactors = FALSE)
    return(j)
  }


  # i = 1
  # x = cr[[i]]

  core_links = function(x) {
    gen_entries_to_df(x$coredata$link)$df
  }

  start_year = function(x) {
    r = x$`author-profile`$`date-created`
    if (is.null(r)) {
      return(r)
    }
    r = no_at(r)
  }
  # Quick setup function
  auth_get_info = function(x){
    # print(i)
    this_core_links = core_links(x)
    core = get_core(x)
    core = c(core, affil = curr_affil(x), run_years(x),
             pref_name(x))

    onames = other_names(x)

    # merge subject areas and frequency
    sa = subjareas(x)
    class_freq = classes(x)
    if (!is.null(sa) && !is.null(class_freq)) {
      sa = merge(sa, class_freq, by = "code", all = TRUE)
    }
    if (is.null(sa) && !is.null(class_freq)) {
      sa = class_freq
    }
    if (!is.null(sa) && is.null(class_freq)) {
      sa = sa
    }

    ahist = affil_hist(x)

    journ = journals(x)
    L = list(info = core,
             links = this_core_links,
             other_names = onames,
             subject_areas = sa,
             affiliation_history = ahist,
             journals = journ)
    # i <<- i + 1
    return(L)
  }

  info = lapply(cr, auth_get_info)
  if (length(info) != length(au_id)) {
    warning("Output not the same length as au_id, not naming the list")
  } else {
    names(info) = au_id
  }
  return(info)
}