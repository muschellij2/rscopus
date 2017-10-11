#' @title List of SCOPUS Entries to List of Affiliations Data Frames
#' @description Take a SCOPUS entry and transform it to a data frame of
#' affiliations
#' @param affils List of affiliations, from \code{\link{entries_to_affil_list}}
#' @return A \code{data.frame} of affiliation information.  A column named
#' \code{"index"} denotes the element of the object \code{affils} that the row
#' corresponds to
#' @export
#' @importFrom stats reshape
affil_list_to_df = function(affils) {

  ########################
  # Get the index (first, second, third affil)
  ########################
  affils = lapply(affils, function(x){
    x$seq = as.numeric(as.character(x$seq))
    x$ind = unlist(tapply(x$seq, x$seq, function(y) {
      seq(length(y))
      }))
    x
  })

  n_reps = sapply(affils, function(x){
    max(table(x$seq))
  })

  ncols = max(n_reps)

  idvars = c("seq", "au_id", "name")
  all_colnames = c(idvars, "ind", "affid", "affilname")
  check = sapply(affils, function(x) {
    all(colnames(x) %in% all_colnames)
  })
  if (!all(check)) {
    stop(paste0("colnames of affils has changed! ",
                "Must be ", paste0(all_colnames, collapse = ", ")))
  }
  ########################
  # Make it wide
  ########################
  auths = lapply(affils, function(x){
    reshape(x,
            timevar = "ind",
            idvar = idvars,
            direction = "wide", sep = "_")
  })

  new_colnames = c(idvars, c(outer(c("affid", "affilname"),
                                 seq(ncols),
                                 paste, sep = "_"))
                                 )
  ########################
  # Make all df same number of cols
  ########################
  auths = lapply(auths, function(x){
    cn = colnames(x)
    sd = setdiff(new_colnames, cn)
    if (length(sd) > 0) {
      mat = matrix(NA, nrow = nrow(x), ncol = length(sd))
      colnames(mat) = sd
      x = cbind(x, mat)
    }
    x = x[, new_colnames]
    return(x)
  })

  ######################
  # Create indexer
  ######################
  auths = mapply(function(x, y) {
    x$index = y
    x
  }, auths, seq_along(auths), SIMPLIFY = FALSE)

  auths = do.call("rbind", auths)

  return(auths)

}
