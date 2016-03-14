
#' @title Find all affiliations
#' @description Take a SCOPUS entry and transform it to a data frame of
#' all affiliations listed in there
#' @param entries list of entries from SCOPUS,
#' usually from \code{\link{author_search}} result
#' @return A \code{data.frame} of affiliations
#' @export
all_possible_affils = function(entries) {

  all_possible_affils = lapply(entries, function(x){

    affs = t(sapply(x$affiliation, function(y){
      c(affid = nonull(y$afid),
        affilname = nonull(y$affilname, replace = ""))
    }))
    affs = as.data.frame(affs,
                         stringsAsFactors = FALSE)
    affs = unique(affs)
  })
  all_possible_affils = do.call('rbind', all_possible_affils)
  all_possible_affils = all_possible_affils[
    !is.na(all_possible_affils$affid), , drop = FALSE]
  all_possible_affils = unique(all_possible_affils)
  return(all_possible_affils)
}