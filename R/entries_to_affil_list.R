#' @title List of SCOPUS Entries to List of Affilations Data Frames
#' @description Take a SCOPUS entry and transform it to a data frame of
#' affiliations
#' @param entries list of entries from SCOPUS,
#' usually from \code{\link{author_search}} result
#' @return A \code{data.frame} of affiliation information
#' @export
entries_to_affil_list = function(entries) {

  all_affils = all_possible_affils(entries)

  auths = lapply(entries, function(x){
    entry_to_affil(x, all_affils)
  })

  return(auths)

}


#' @title SCOPUS Entry to Affiliation
#' @description Take an individual SCOPUS entry and transform it to a data frame of
#' affiliations
#' @param x individual entry from SCOPUS,
#' usually from \code{\link{author_search}}
#' @param all_affils Affilation data.frame from \code{\link{all_possible_affils}}
#' @return A \code{data.frame} of affiliation information
#' @export
entry_to_affil = function(x, all_affils) {

  ###################################
  # Get individual affiliations from each person
  ###################################
  people_affils = lapply(x$author, function(y){
    if (is.null(y$afid)) {
      return(data.frame(affid = NA, affilname = NA))
    }
    affs = lapply(y$afid, function(r){
      xx = data.frame(affid = nonull(r$`$`), stringsAsFactors = FALSE)
      if (all(is.na(xx$affid))) {
        return(data.frame(affid = NA, affilname = NA))
      }
      xx = merge(xx, all_affils, all.x = TRUE)
    })
    affs = do.call("rbind", affs)
    affs = unique(affs)
  })

  ###################################
  # Get the people
  ###################################
  persons = lapply(x$author, function(y){
    y = y[c("@seq", "authid",
            "given-name", "surname")]
    y = unlist(y)
    name = paste(y["given-name"], y['surname'])
    y = y[c("@seq", "authid")]
    y = c(y, name = name)
    names(y) = c("seq", "au_id", "name")
    y = as.data.frame(t(y), stringsAsFactors = FALSE)
    return(y)
  })

  ###################################
  # Merge them together
  ###################################
  res = mapply(function(person, affils){
    x = merge(person, affils, all = TRUE)
  }, persons, people_affils, SIMPLIFY = FALSE)


  ##############################
  # Make one large df
  ##############################
  res = do.call("rbind", res)
  res = unique(res)
  return(res)
}




#' @title Collapse Affiliations to one row
#' @description Take an individual SCOPUS entry and transform it to a data frame of
#' affiliations
#' @param affils Data frame of affiliations to be collapsed
#' usually from \code{\link{author_search}}
#' @param collapse What should values be collapsed using as a separator
#' @return A \code{data.frame} of affiliation information
#' @export
#' @importFrom plyr ddply .
collapse_affil = function(affils, collapse = ";") {
  # global variable crap
  index = NULL
  rm(list = "index")
  ###################################
  # Get individual affiliations from each person
  ###################################
  df = affil_list_to_df(affils)
  # cn = colnames(df)
  df$seq = NULL

  df[is.na(df)] = ""

  df = ddply(df, .(index), function(x){
    x = lapply(x, function(y) {
      paste(y, collapse = collapse)
    })
    x = as.data.frame(x)
    x$index = NULL
    x
  })
  df
}
