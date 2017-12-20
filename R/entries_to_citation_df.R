#' @title List of SCOPUS Entries to Data Frame of Citations
#' @description Take a SCOPUS entry list and transform it to a data frame of
#' citation and article information
#' @param entries list of entries from SCOPUS,
#' usually from \code{\link{author_search}} result
#' @return A \code{data.frame} of citation information
#' @export
entries_to_citation_df = function(entries){
  ##################################
  # Get Citation Information
  ##################################
  cites = sapply(entries, function(x){
    x = as.numeric(nonull(x$`citedby-count`))
    x
  })

  ##################################
  # Get Journal Information
  ##################################
  journals = sapply(entries, function(x){
    x = nonull(x$`prism:publicationName`)
    x
  })

  ##################################
  # Get Description of type of entry (article/presentation)
  ##################################
  desc = sapply(entries, function(x){
    x = nonull(x$subtypeDescription)
    x
  })
  ##################################
  # Get Dates of Publication
  ##################################
  dates = t(sapply(entries, function(x){
    cx = nonull(x$`prism:coverDate`)
    cdx = nonull(x$`prism:coverDisplayDate`)
    c(cover_date = cx,
      cover_display_date = cdx)
  }))

  ##################################
  # Get Titles of Publication
  ##################################
  titles = sapply(entries, function(x){
    x = nonull(x$`dc:title`)
  })

  ##################################
  # Get pii number to grab pdf later
  ##################################
  sci_pii = sapply(entries, function(x){
    x = nonull(x$pii)
  })

  ##################################
  # Get DOI
  ##################################
  doi = sapply(entries, function(x){
    x = nonull(x$"prism:doi")
  })

  eid = sapply(entries, function(x){
    x = nonull(x$eid)
  })


  ##################################
  # Put all into a data.frame
  ##################################
  df = data.frame(citations = cites,
                  journal = journals,
                  description = desc,
                  title = titles,
                  pii = sci_pii,
                  doi = doi,
                  eid = eid,
                  stringsAsFactors = FALSE
  )
  df = cbind(df, dates)

  # commented out are the ones we have
  all_types = c("prism:url", "dc:identifier",
                # "eid",
                # "dc:title",
                "dc:creator",
                # "prism:publicationName",
                "prism:issn", "prism:eIssn",
                "prism:pageRange",
                # "prism:coverDate", "prism:coverDisplayDate",
                # "prism:doi",
                "dc:description",
                # "citedby-count",
                "prism:aggregationType",
                "subtype",
                # "subtypeDescription",
                "authkeywords",
                "source-id")
  all_names = gsub(":", "_", all_types)
  all_names = gsub("-", "_", all_names)

  res = sapply(all_types, function(type) {
    x = sapply(entries, function(ent) {
      nonull(ent[[type]])
    })
    return(x)
  })
  if (is.matrix(res)) {
    colnames(res) = all_names
    res = data.frame(res, stringsAsFactors = FALSE)
    df = cbind(df, res)
  }

  return(df)
}