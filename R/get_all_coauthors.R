
#' Get all Co-Authors
#'
#' @param ... arguments to pass to \code{\link{author_df}}
#'
#' @return A list of output, including a vector of author_ids
#' @export
#'
#' @examples
#'  \dontrun{
#'  get_all_coauthors(last_name = "muschelli")
#'  }
get_all_coauthors = function(...) {

  make_df = function(df) {
    for (i in seq_along(df)) {
      r = df[[i]]
      r = lapply(r, function(xx) {
        if (length(xx) == 0) {
          xx = NA
        }
        xx
      })
      L = sapply(r, length)
      if (all(L ==  1)) {
        df[[i]] = unlist(r)
      }
    }
    df
  }

  #######################################################
  # Find an author information
  #######################################################
  res = author_df(...)
  # mydata = get_complete_author_info(last_name = "Muschelli")
  scopus_ids = sub("SCOPUS_ID:", "", res$`dc:identifier`)

  scopus_id = scopus_ids[1]
  get_list = function(scopus_id, verbose = TRUE) {
    out = rscopus::abstract_retrieval(scopus_id, verbose = verbose)
  }
  if (requireNamespace("pbapply", quietly = TRUE)) {
    output = pbapply::pblapply(scopus_ids, get_list, verbose = FALSE)
  } else {
    output = lapply(scopus_ids, get_list, verbose = TRUE)
  }

  for (i in 1:20) {
    output = mapply(
      function(out, scopus_id) {
        if (httr::status_code(out$get_statement) >= 400) {
          message("Waiting on rate limit")
          out = get_list(scopus_id, verbose = TRUE)
        }
        out
      }, output, scopus_ids, SIMPLIFY = FALSE)
  }


  output = lapply(output, function(out) {
    author_df = jsonlite::fromJSON(
      jsonlite::toJSON(out$content$`abstracts-retrieval-response`$authors),
      flatten = TRUE)$author
    list(
      full_output = out,
      author_df = author_df
    )
  })


  names(output) = scopus_ids

  all_authors = lapply(output, function(x) x$author_df)
  all_authors = dplyr::bind_rows(all_authors, .id = "scopus_id")
  all_authors$au_id = sapply(all_authors$`@auid`, function(x)  {
    stopifnot(length(x) ==1)
    c(x)
  })
  all_authors$affiliation = lapply(all_authors$affiliation, function(x) {
    if (is.list(x)) {
      x = dplyr::as_tibble(x)
    }
    if (is.data.frame(x)) {
      x = unique(x)
    }
    x
  })
  all_authors$affiliation_id = mapply(function(x, y) {
    unique(c(x, unlist(y$`@id`)))
  }, all_authors$`affiliation.@id`, all_authors$affiliation, SIMPLIFY = FALSE)



  affils = lapply(output, function(x) {
    x = x$full_output$content
    x = x[["abstracts-retrieval-response"]]
    out = dplyr::as_tibble(
      jsonlite::fromJSON(
        jsonlite::toJSON(x$affiliation),
        flatten = TRUE)
    )
    out = make_df(out)
    out
  })
  affils = dplyr::bind_rows(affils, .id = "scopus_id")
  affils = affils %>%
    dplyr::select(affiliation_id = "@id",
                  affiliation_name = "affilname",
                  affiliation_scopus_url = "@href",
                  affiliation_country = "affiliation-country") %>%
    dplyr::distinct()


  run_authors = function(all_authors) {
    affiliation_number = affiliation_id = au_id = NULL
    rm(list = c("affiliation_id", "au_id", "affiliation_number"))
    affil_df = all_authors %>%
      dplyr::select(
        scopus_id,
        au_id,
        author_order = "@seq",
        indexed_name = "ce:indexed-name",
        given_name = "ce:given-name",
        surname = "ce:surname",
        author_url = "author-url",
        affiliation_id
      ) %>%
      dplyr::as_tibble()
    affil_df = make_df(affil_df)
    n_total_affil = max(sapply(affil_df$affiliation_id, length))
    affil_df$affiliation_id = sapply(affil_df$affiliation_id, function(x) {
      if (length(x) == 0) x = NA
      if (all(is.na(x))) return(x)
      paste(x, collapse = ",")
    })

    affil_cols =  paste0("affil_", 1:n_total_affil)
    affil_df = affil_df %>%
      tidyr::separate("affiliation_id",
                      into = affil_cols,
                      fill = "right",
                      extra = "merge",
                      sep = ",") %>%
      tidyr::gather("affiliation_number", "affiliation_id", !!!affil_cols) %>%
      dplyr::mutate(affiliation_number = as.numeric(sub("affil_", "",
                                                       affiliation_number)))
  }
  affil_df = try({run_authors(all_authors)})
  if (inherits(affil_df, "try-error")) {
    affil_df = NULL
  }


  collaborators = sort(unique(all_authors$au_id))
  out = list(
    collaborator_au_ids = collaborators,
    author_data = res,
    all_author_data = all_authors,
    all_output = output,
    affiliation_data = affils
)
  out$author_affiliation_data = affil_df
  out
}

