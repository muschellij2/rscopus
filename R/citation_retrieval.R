#' SCOPUS Citation Retrieval
#'
#' @param scopus_id Scopus Identifier
#' @param pubmed_id Scopus Identifier
#' @param pii Scopus Identifier
#' @param doi Scopus Identifier
#' @param date_range date range to specify, must be length 2
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' @export
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = Sys.getenv("Elsevier_API_Interactive")
#' set_api_key(api_key)
#' if (!is.null(api_key) & nchar(api_key) > 0){
#'    result = citation_retrieval(pii = c("S0140673616324102",
#'    "S0014579301033130"),
#'    verbose = FALSE)
#'    if (httr::status_code(result$get_statement) < 400) {
#'       res = parse_citation_retrieval(result)
#'    }
#'
#' }
#' set_api_key(NULL)
citation_retrieval <- function(
  scopus_id = NULL,
  pii = NULL,
  doi = NULL,
  pubmed_id = NULL,
  date_range = NULL,
  ...
){


  args = list(type = "citations", ...)
  func = function(x) {
    if (!is.null(x) & length(x) > 0) {
      if (length(x) > 25) {
        warning("Number of documents > 25 - may not retrieve all results")
      }
      x = paste(x, collapse = ",")
    }
    return(x)
  }
  if (!is.null(date_range)) {
    stopifnot(length(date_range) == 2)
    date = trimws(date_range)
    date = paste(date, collapse = "-")
    args$date = date
  }
  if (!is.null(scopus_id)) {
    scopus_id = gsub("SCOPUS_ID:", "", scopus_id, fixed = TRUE)
  }
  scopus_id = func(scopus_id)
  args$scopus_id = scopus_id
  pii = func(pii)
  args$pii = pii
  if (!is.null(doi)) {
    doi = gsub("DOI:", "", doi, fixed = TRUE)
  }
  doi = func(doi)
  args$doi = doi
  pubmed_id = func(pubmed_id)
  args$pubmed_id = pubmed_id
  s = do.call(generic_elsevier_api, args = args)

  return(s)

}



#' @export
#' @param result result from \code{\link{citation_retrieval}}, which
#' has an element of \code{content}
#' @rdname citation_retrieval
parse_citation_retrieval = function(result) {
  x = result$content$`abstract-citations-response`
  ident = x$`identifier-legend`$identifier
  ident = lapply(ident, function(z) {
    lapply(z, function(r){
    if (is.null(r)) {
      r = NA
    }
    return(r)
    })
  })
  ident = lapply(ident, as.data.frame, stringsAsFactors = FALSE)
  names(ident) = 1:length(ident)
  ident = bind_rows(ident, .id = "identifier")
  cinfo = x$citeInfoMatrix$citeInfoMatrixXML$citationMatrix$citeInfo
  hdr = x$citeColumnTotalXML$citeCountHeader
  authors = lapply(cinfo, function(r) {
    auth = bind_rows(lapply(r$author, as.data.frame,
                            stringsAsFactors = FALSE))
  })
  names(authors) =  1:length(authors)
  authors = bind_rows(authors, .id = "identifier")

  chead = hdr$columnHeading
  chead = unlist(chead)
  cinfo = lapply(cinfo, function(r) {
    r$author = NULL
    r$citationType = unlist(r$citationType)
    r$cc = unlist(r$cc)
    if (is.null(r$cc)) {
      r$cc = rep(NA, length = length(chead))
    }
    names(r$cc) = chead
    r = unlist(r)
    as.data.frame(t(r), stringsAsFactors = FALSE)
  })
  names(cinfo) =  1:length(cinfo)
  cinfo = bind_rows(cinfo, .id = "identifier")

  ctot = hdr$columnTotal
  ctot = unlist(ctot)
  names(ctot) = chead
  hdr$columnTotal = ctot
  hdr$columnHeading = NULL
  L = list(authors = authors,
           citation_info = cinfo,
           header = hdr,
           identifiers = ident)
  return(L)
}