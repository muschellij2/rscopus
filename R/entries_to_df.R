
#' @title Convert Entries into a data.frame
#'
#' @description Converts a list of entries into a \code{data.frame} of records
#' @param entries Author ID number. Overrides any first/last name argument
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param verbose Print diagnostic messages
#' @param ... Arguments to be passed to \code{\link{author_search}}
#' @export
#' @return Data frame of records
#' @import plyr
entries_to_df = function(entries, au_id = NULL, verbose = TRUE) {

  # Getting number of affilations to push out
  n_affils = sapply(entries, function(x){
    length(x$affiliation)
  })

  ##################################
  # Get affiliation information
  ##################################
  # Need to make sure 1 for consistency in df
#   max_n_affils = max(max(n_affils), 1)
#   affils = t(sapply(entries, function(x){
#     x = sapply(x$affiliation, function(y){
#       nonull(y$affilname, replace = "")
#     })
#     x = c(x, rep("",
#                  max_n_affils - length(x)))
#   }))
#
#   # replace all missing with NA
#   affils[affils %in% ""] = NA
#   colnames(affils) = paste0("affil_",
#                             seq(ncol(affils)))
#   affils = as.data.frame(affils,
#                          stringsAsFactors = FALSE)
#
  ### Get All possible affiliations from collaborators
  all_possible_affils = all_possible_affils(entries)

  auths = llply(entries, function(x){

    res = entry_to_affil(x = x,
                         all_affils = all_possible_affils)

    n_authors = max(as.numeric(res$seq))
    # print(n_authors)
    rres = res

    # affil_list_to_df[[1]]
    if (!is.null(au_id)) {
      rres = res[ res$au_id %in% au_id, , drop = FALSE]
      auth_order = unique(as.numeric(rres$seq))
      if (nrow(rres) == 0) {
        auth_order = rep(NA, n_authors)
      }

      f_res = data.frame(
        auth_order = auth_order,
        stringsAsFactors = FALSE
      )

      mat = t(rres$affilname)
      colnames(mat) = paste0("affilname_", 1:ncol(mat))

      rres = cbind(f_res, mat)
      rres = unique(rres)
#       if (ncol(rres) > 3) {
#         colnames(rres)[3:ncol(rres)] = paste0("affil_", 2:(ncol(rres) - 2) )
#         # print(rres)
#       }
      if (nrow(rres) == 0) {
        # print(res)
      }
    } else {
      rres$seq = NULL

      rres[is.na(rres)] = ""

      rres = lapply(rres, function(y) {
        paste(y, collapse = ";")
      })
      rres = as.data.frame(rres)
      rres$index = NULL

    }

    rres$n_auth = n_authors


    return(rres)
  }, .progress = ifelse(verbose, "text", "none"))

  new_colnames = unlist(lapply(auths, colnames))
  new_colnames = unique(new_colnames)

  auths = llply(auths, function(x){
    cn = colnames(x)
    sd = setdiff(new_colnames, cn)
    if (length(sd) > 0) {
      mat = matrix(NA, nrow = nrow(x), ncol = length(sd))
      colnames(mat) = sd
      x = cbind(x, mat)
    }
    x = x[, new_colnames]
    return(x)
  }, .progress = ifelse(verbose, "text", "none"))

  df = do.call("rbind", auths)
  df$n_affils = n_affils
  cites = entries_to_citation_df(entries = entries)
  df = cbind(df, cites)

  return(df)
}