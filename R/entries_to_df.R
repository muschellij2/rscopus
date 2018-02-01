
#' @title Convert Entries into a data.frame
#'
#' @description Converts a list of entries into a \code{data.frame} of records
#' @param entries Entries from the output of a command
#' @param au_id Author ID number. Overrides any first/last name argument
#' @param verbose Print diagnostic messages
#' @export
#' @return Data frame of records
#' @importFrom plyr llply
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
      if (ncol(mat) > 0) {
        colnames(mat) = paste0("affilname_", 1:ncol(mat))
      }

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






#' @rdname entries_to_df
#' @importFrom dplyr left_join full_join bind_rows
#' @importFrom dplyr as_tibble
#' @importFrom tidyr nest
#' @export
entries_to_df2 = function(entries, verbose = TRUE) {



  df = lapply(
    entries, function(ent) {

      ent$link = NULL
      ent$author_count_limit =
        ent$`author-count`$`@limit`
      ent$author_count =
        ent$`author-count`$`$`
      ent$`author-count` = NULL


      author = ent$author
      if (!is.null(author)) {
        auth = bind_list(author)
        affil = ent$affil

        cn = colnames(auth)
        run = "afid.$" %in% cn
        if (!run) {
          auth$afid = NA_character_
        } else {
          cn[ cn == "afid.$" ] = "afid"
          colnames(auth) = cn
        }
        if (!is.null(affil)) {
          affil = lapply(affil, function(x) {
            lapply(x, nonull)
          })
          affil = bind_list(affil)
          affil$`@_fa` = auth$`@_fa` = NULL
          auth = dplyr::left_join(auth, affil, by = "afid")
        }
        auth = dplyr::as_tibble(auth)
      }
      ent$author = ent$affiliation = NULL
      ent = lapply(ent, nonull)

      cn = names(ent)
      ddf = as.data.frame(ent, stringsAsFactors = FALSE)
      colnames(ddf) = cn
      ddf = dplyr::as_tibble(ddf)
      cn = colnames(ddf)
      id = NULL
      rm(list = "id")
      if (!is.null(author)) {
        ddf$id = auth$id = 1
        auth = unique(auth)
        auth = tidyr::nest(auth, -id)
        ddf = dplyr::full_join(auth, ddf, by = "id")
        ddf = dplyr::as_tibble(ddf)
        ddf$id = NULL
      }
      ddf
    })

  df = lapply(seq_along(df), function(i) {
    df[[i]]$id = i
    df[[i]]
  })

  df = dplyr::bind_rows(df)
  return(df)
}