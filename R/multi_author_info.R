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

  au_id = paste(au_id, collapse = ",")
  res = generic_elsevier_api(
    author_id = au_id,
    verbose = verbose,
    type = "author",
    api_key = api_key,
    ...)
  res$au_id = au_id
  return(res)
}


#' Get Relevant Authors Information from IDs from Scopus
#'
#' @param ... Arguments passed to \code{\link{get_complete_author_info}}
#' @seealso \code{\link{get_complete_author_info}}
#' @return Data.frame of information
#' @examples \dontrun{
#' multi_author_info(au_id =  "22968535800", "40462056100"))
#' }
#' @export
multi_author_info <- function(...){
  res = complete_multi_author_info(...)
  cr = res$content
  au_id = res$au_id
  au_id = strsplit(au_id, split = ",")[[1]]
  au_id = au_id[ !au_id %in% "" ]
  cr = cr$`author-retrieval-response-list`$`author-retrieval-response`

  no_at = function(y) {
    cn = names(y)
    cn = gsub("@", "", cn, fixed = TRUE)
    names(y) = cn
    y
  }

  get_core = function(x) {
    cd = x$coredata
    sd = setdiff(names(cd), "link")
    cd = cd[sd]
    cd = unlist(cd)
    return(cd)
  }

  make_affil = function(affil) {
    affil = unlist(affil)
    affil = no_at(affil)
    cn = names(affil)
    cn = sub("\\$$", "", cn)
    cn = sub("[.]$", "", cn)
    names(affil) = cn
    affil
  }
  curr_affil = function(x) {
    affil = x$`author-profile`$`affiliation-current`$affiliation
    make_affil(affil)
  }

  affil_hist = function(x) {
    affil = sapply(x$`author-profile`$`affiliation-history`$affiliation,
                   make_affil)
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

  }

  # sapply(cr, function(x) sapply(x$`author-profile`$`affiliation-history`$affiliation, unlist))

  subjareas = function(x) {
    sa = x$`subject-areas`$`subject-area`
    sa = t(sapply(sa, unlist))
    cn = colnames(sa)
    cn = setdiff(cn, "@_fa")
    sa = sa[, cn]
    cn = gsub("@", "", cn, fixed = TRUE)
    cn[ cn == "$"] = "longname"
    colnames(sa) = cn
    sa = as.data.frame(sa, stringsAsFactors = FALSE)
    return(sa)
  }

  classes = function(x) {
    class_freq = x$`author-profile`$classificationgroup
    class_freq = class_freq$classifications$classification
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
    y = unlist(y)
    y = no_at(y)
    y
  }

  pref_name = function(x) {
    y = x$`author-profile`$`preferred-name`
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
    r = sapply(r, function(y) {
      unlist(y)
      y = no_at(y)
      keep = c("doc-count", "initials",
               "indexed-name", "surname",
               "given-name")
      y = y[keep]
      names(y) = keep
      y
    })
    r = t(r)
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
    return(j)
  }


  # Quick setup function
  auth_get_info = function(x){

    core = get_core(x)
    core = c(core, affil = curr_affil(x), run_years(x),
             pref_name(x))

    onames = other_names(x)

    # merge subject areas and frequency
    sa = subjareas(x)
    class_freq = classes(x)
    sa = merge(sa, class_freq, by = "code", all = TRUE)

    ahist = affil_hist(x)

    journ = journals(x)
    L = list(info = core,
             other_names = onames,
             subject_areas = sa,
             affiliation_history = ahist,
             journals = journ)
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

