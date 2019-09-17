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
#'    x = abstract_retrieval("S1053811915002700", identifier = "pii",
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

  self =  content$`abstracts-retrieval-response`$coredata
  year = substr(self$`prism:coverDate`, 1, 4)
  title = self$`dc:title`
  split_title = strsplit(title, " ")[[1]]
  first = split_title[1]
  last = split_title[length(split_title)]

  first_auth_last_name = authors$`ce:surname`[1]
  key = paste0(first_auth_last_name, year, first, last)

  abstract = self$`dc:description`

  authors = paste(authors$`ce:given-name`, authors$`ce:surname`)
  authors = paste(authors, collapse = " and ")

  address = (paste(content$`abstracts-retrieval-response`$affiliation$affilname, collapse = ";"))

  pages = self$`prism:pageRange`
  if (is.null(pages)) {
    if (!is.null(self$`prism:startingPage`) & !is.null(self$`prism:endingPage`)) {
      pages = paste0(self$`prism:startingPage`, "-", self$`prism:endingPage`)
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
  bib = glue::glue(paste(" <key>,",
                   "  author = {<auth>},",
                   "  address = {<address>},",
                   "  title = {<title>},",
                   "  journal = {<jour>},",
                   "  year = {<year>},",
                   "  volume = {<vol>},",
                   "  number = {<number>},",
                   "  pages = {<pages>},",
                   "  doi = {<doi>}",
                   "  abstract = {<abstract>}",
                   sep = "\n"),
             key=key,
             auth=authors,
             address=address,
             title=title,
             year=year,
             jour=self$`prism:publicationName`,
             vol= self$`prism:volume`,
             number=self$`prism:issueIdentifier`,
             pages=pages,
             doi = self$`prism:doi`,
             abstract = abstract,
             .open = "<", .close = ">")
  bib = paste0("@article{", bib, "}")

}