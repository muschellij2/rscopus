#' Makes a \code{bibtex} entry from an output of
#' \code{\link{abstract_retrieval}} or \code{\link{article_retrieval}}
#'
#' @param x output of \code{\link{abstract_retrieval}} or
#' \code{\link{article_retrieval}}, with both \code{get_statement}
#' and \code{content}
#'
#'
#' @return A character vector of bibtex values
#' @export
#' @note Adapted from
#' \url{https://github.com/pybliometrics-dev/pybliometrics/blob/master/pybliometrics/scopus/abstract_retrieval.py}
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = abstract_retrieval("S1053811915002700", identifier = "pii",
#'    verbose = FALSE)
#'    res = bibtex_core_data(x)
#'    cat(res)
#'    x = abstract_retrieval("84929707579", identifier = "scopus_id",
#'    verbose = FALSE)
#'    res2 = bibtex_core_data(x)
#'    cat(res2)
#' }
bibtex_core_data = function(x) {
  # adapted from
  # https://github.com/scopus-api/scopus/blob/master/scopus/abstract_retrieval.py#L598
  content = httr::content(x$get_statement, as = "text")
  content = jsonlite::fromJSON(content, flatten = TRUE)
  authors = content$`abstracts-retrieval-response`$authors$author
  if (is.null(authors)) {
    authors = content$`abstracts-retrieval-response`$coredata$`dc:creator`$author
  }
  self =  content$`abstracts-retrieval-response`$coredata
  if (is.null(self)) {
    warning("No coredata given, returning NULL")
    return(NULL)
  }
  if (is.null(self)) {
    self = content$`full-text-retrieval-response`$coredata
    dc_creator = self$`dc:creator`
    colnames(dc_creator)[ colnames(dc_creator) == "$"] = "author"
    dc_create_auth = dc_creator$author
    if (!is.null(dc_create_auth)) {
      if (is.vector(dc_create_auth)) {
        dc_create_auth = strsplit(dc_create_auth, split = ", ")
        dc_create_auth = do.call(rbind, dc_create_auth)
        colnames(dc_create_auth) = c("ce:surname", "ce:given-name")
        dc_create_auth = as.data.frame(dc_create_auth,
                                       stringsAsFactors = FALSE)
        dc_creator = list(author = dc_create_auth)
      }
      self$`dc:creator` = dc_creator
    }
  }
  bad_authors = FALSE
  if (is.null(authors)) {
    warning(
      paste0("Authors are NULL, output should be from abstract_retrieval? ",
             "Author list may not be right!")
    )
    bad_authors = TRUE
    authors = self$`dc:creator`$author
  }
  year = substr(self$`prism:coverDate`, 1, 4)
  title = self$`dc:title`
  if (!is.null(title)) {
    title = trimws(title)
    split_title = strsplit(title, " ")[[1]]
    split_title = gsub("[[:punct:]]", "", split_title)
  } else {
    warning("Title is NULL!")
    split_title = NA_character_
  }
  first = split_title[1]
  last = split_title[length(split_title)]

  first_auth_last_name = authors$`ce:surname`[1]
  key = paste0(first_auth_last_name, year, first, last)

  abstract = self$`dc:description`
  if (is.null(abstract)) {
    abstract = ""
  }

  authors = paste(authors$`ce:given-name`, authors$`ce:surname`)
  authors = paste(authors, collapse = " and ")
  if (bad_authors && is.null(authors)) {
    authors = ""
  }

  address = paste(
    content$`abstracts-retrieval-response`$affiliation$affilname,
    collapse = "; ")

  pages = self$`prism:pageRange`
  if (is.null(pages)) {
    if (!is.null(self$`prism:startingPage`) &
        !is.null(self$`prism:endingPage`)) {
      pages = paste0(self$`prism:startingPage`, "-",
                     self$`prism:endingPage`)
    } else {
      pages = "-"
    }
  }
  make_names = c("prism:publicationName", "prism:doi", "prism:volume",
                 "prism:issueIdentifier")
  for (iname in make_names) {
    if (is.null(self[[iname]])) {
      self[[iname]] = ""
    }
  }
  # jour=self$`prism:publicationName`,
  # vol= self$`prism:volume`,
  # number=self$`prism:issueIdentifier`,
  # self$`prism:volume` = ""
  # self$`prism:issueIdentifier` = ""

  # All information
  L = list(key=key,
           auth=authors,
           address=address,
           title=title,
           year=year,
           jour=self$`prism:publicationName`,
           vol= self$`prism:volume`,
           number=self$`prism:issueIdentifier`,
           pages=pages,
           doi = self$`prism:doi`,
           abstract = abstract)
  L = lapply(L, function(x) {
    if (is.null(x)) x = ""
    x
  })
  L = list2env(L)
  bib = glue::glue(
    paste(" <key>,",
          "  author = {<auth>},",
          "  address = {<address>},",
          "  title = {<title>},",
          "  journal = {<jour>},",
          "  year = {<year>},",
          "  volume = {<vol>},",
          "  number = {<number>},",
          "  pages = {<pages>},",
          "  doi = {<doi>},",
          "  abstract = {<abstract>}",
          sep = "\n"),
    .envir = L,
    .open = "<", .close = ">")
  bib = paste0("@article{", bib, "}")

}
