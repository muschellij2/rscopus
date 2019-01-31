#' @title ScienceDirect Object Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to
#' give a
#' retrieval of an object from the Elsevier Object Retrieval API
#' @param id Identifier for object
#' @param identifier Type of identifier to use
#' @param ref document reference
#' @param http_end any additional end to http statement.
#' See \code{\link{generic_elsevier_api}}
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' or \code{\link{GET}}
#' @export
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = object_retrieval("S1053811915002700", identifier = "pii",
#'    verbose = FALSE)
#'    df = process_object_retrieval(x)
#'    df = df[ grepl("image/jpeg", df$mime_type),,drop = FALSE ]
#'    df = df[ df$type %in% "IMAGE-HIGH-RES",,drop = FALSE ]
#'    res = download_object(df$url[1])
#'    if (interactive()) {
#'       browseURL(res$outfile)
#'    } else {
#'      img = res$content
#'      dims = dim(img)[1:2]
#'      mdim = max(dims)
#'      graphics::plot(c(0, ncol(img)), c(0, nrow(img)), type='n')
#'      graphics::rasterImage(img, 1, 1, ncol(img), nrow(img))
#'    }
#' }
object_retrieval <- function(
  id, # Identifier for abstract
  identifier = c("scopus_id", "eid", "doi", "pii", "pubmed_id"),
  ref = NULL,
  http_end = NULL,
  ...
){

  identifier = match.arg(identifier)
  id = gsub("SCOPUS_ID:", "", id, fixed = TRUE)
  id = gsub("DOI:", "", id, fixed = TRUE)

  ender = paste(identifier, id, sep = "/")
  if (!is.null(ref)) {
    ender = paste(ender, "ref", ref, sep = "/")
  }
  if (!is.null(http_end)) {
    ender = paste(ender, http_end, sep = "/")
  }
  ################################
  #
  ################################
  #   arguments <- list(...)
  #   n_args = names(arguments)
  #   l_args = length(arguments)
  #
  #   if (length(n_args) != l_args){
  #     warning(paste0("All arguments should be named in ..., ",
  #     "may give unpredictable results")
  #   }
  #   if ("http_end" %in% n_args) {
  #     ender = paste(http_end, ender, sep = "/")
  #   }
  s = generic_elsevier_api( type = "object",
                            http_end = ender, ...)
  return(s)

}



#' @export
#' @param res result from \code{\link{object_retrieval}}
#' @rdname object_retrieval
#' @importFrom tools file_ext
#' @importFrom httr write_disk
process_object_retrieval = function(res) {
  df = gen_entries_to_df(res$content)$choice
  rownames(df) = NULL
  cn = colnames(df)
  cn[cn == "$"] = "url"
  cn[cn == "@type"] = "type"
  cn[cn == "@ref"] = "ref"
  colnames(df) = cn
  df$mime_type = sub(".*httpAccept=", "", df$url)
  df$no_mime_url = sub("httpAccept=.*", "", df$url)
  df$no_mime_url = sub("\\?$", "", df$no_mime_url)
  df$extension = tools::file_ext(df$no_mime_url)
  return(df)
}


#' @export
#' @param url url to download from \code{\link{object_retrieval}}
#' @param verbose Print messages from specification
#' @param api_key Elsevier API key
#' @param api_key_error Should there be an error if no API key?
#' @param headers Headers passed to \code{\link{add_headers}},
#' passed to \code{\link{GET}}
#' @rdname object_retrieval
download_object = function(
  url,
  api_key = NULL,
  api_key_error = TRUE,
  verbose = TRUE,
  headers = NULL,
  ...) {

  api_key = get_api_key(api_key, error = api_key_error)

  parsed_url = httr::parse_url(url)
  extension = tools::file_ext(parsed_url$path)
  parsed_url$query$APIKey = api_key
  url = httr::build_url(parsed_url)

  if (verbose) {
    parsed_url = httr::parse_url(url)
    parsed_url$query$APIKey = NULL
    parsed_url = httr::build_url(parsed_url)
    message(paste0("HTTP specified is:", parsed_url, "\n"))
  }

  outfile = tempfile(fileext = paste0(".", extension))
  hdrs = do.call(httr::add_headers, args = as.list(headers))
  r = httr::GET(
    url,
    httr::write_disk(path = outfile, overwrite = TRUE),
    hdrs)
  httr::warn_for_status(r)
  cr = content(r)
  return(list(get_statement = r, content = cr,
              outfile = outfile))
}

#' @export
#' @rdname object_retrieval
download_objects = function(
  url,
  ...) {
  lapply(url, download_object, ...)
}