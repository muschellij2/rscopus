#' Generally Convert Entries into a list of \code{data.frame}s
#'
#' @param entries Entries from the output of a command
#' @param scrub Should `scrub_identifier` be run on the identifier?
#' @export
#' @return List of \code{data.frame}s from entries
gen_entries_to_df = function(entries, scrub = FALSE) {
  if ("entries" %in% names(entries)) {
    warning(paste0("You may not be passing in entries, but a list",
                   " of output, which has a entries element"))
  }
  is.named = function(x) {
    if (is.character(x)) {
      return(TRUE)
    }
    if (is.logical(x)) {
      return(TRUE)
    }
    # maybe is.atomic?
    length(names(x)) == length(x)
  }
  ind = 1
  e2 = lapply(entries, function(ent) {
    ent$link =  NULL
    ent = lapply(ent, function(x) {
      if (is.named(x)) {
        unlist(x)
      } else {
        bind_list(x)
      }
    })
    non_df = !sapply(ent, is.data.frame)
    one_df = c(unlist(ent[non_df]), entry_number = ind)
    multi_df = ent[!non_df]
    multi_df = lapply(multi_df, function(r) {
      r$entry_number = ind
      r
    })
    ind <<- ind + 1
    list(one_df = one_df, multi_df = multi_df)
  })
  one_df = lapply(e2, "[[", "one_df")
  one_df = bind_list(one_df)
  if ("dc:identifier" %in% colnames(one_df)) {
    if (scrub) {
      one_df$"dc:identifier" = scrub_identifier(one_df$"dc:identifier")
    }
  }
  L = list(df = one_df)
  multi_df_names = unique(unlist(lapply(e2, function(x){
    names(x$multi_df)
  })))
  multi_df_names = setdiff(multi_df_names, "")
  if (length(multi_df_names) > 0) {

    multi_df = lapply(multi_df_names, function(n) {
      ind = 1
      res = lapply(e2, function(ent) {
        result = ent$multi_df[[n]]
        ind <<- ind + 1
        if (is.null(result)) {
          result = data.frame(entry_number = ind)
        }
        return(result)
      })
      bind_list(res)
    })

    names(multi_df) = multi_df_names
    nrows = sapply(multi_df, nrow)
    multi_df = multi_df[nrows > 0]
    if (sum(nrows > 0) > 0) {
      L = c(L, multi_df)
    }
  }
  return(L)
}