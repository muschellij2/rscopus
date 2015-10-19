#' @title ScienceDirect Object Retrieval
#'
#' @description This function wraps \code{\link{generic_elsevier_api}} to give a
#' retrieval of an object from the Elsevier Object Retrieval API
#' @param id Identifier for object
#' @param identifier Type of identifier to use
#' @param ref document reference
#' @param http_end any additional end to http statement.
#' See \code{\link{generic_elsevier_api}}
#' @param ... Arguments to be passed to \code{\link{generic_elsevier_api}}
#' @export
#' @import httr
#' @seealso \code{\link{generic_elsevier_api}}
#' @return List of elements, similar to \code{\link{generic_elsevier_api}}
#' @examples
#' api_key = get_api_key(NULL, error = FALSE)
#' if (!is.null(api_key)){
#'    x = object_retrieval("S1053811915002700", identifier = "pii")
#'    if (require(xml2) & require(httr)){
#'        library(httr)
#'        refs = httr::content(x$get_statement, "text")
#'        refs = read_xml(refs)
#'        refs = xml_nodes(refs, "choice")
#'        texts = xml_text(refs)
#'        types = xml_attr(refs, "type")
#'        refs = xml_attr(refs, "ref")
#'        df = data.frame(ref = refs, type = types, text = texts,
#'        stringsAsFactors = FALSE)
#'        df = df[ grepl("image/jpeg", df$text),,drop = FALSE ]
#'        df = df[ df$type %in% "IMAGE-HIGH-RES",,drop = FALSE ]
#'        r = GET(df$text[1],
#'                query = list(
#'                  "apiKey" = api_key))
#'        img = content(r)
#'        dims = dim(img)[1:2]
#'        mdim = max(dims)
#'        plot(c(0, ncol(img)), c(0, nrow(img)), type='n')
#'        rasterImage(img, 1, 1, ncol(img), nrow(img))
#'    }
#'  obj = object_retrieval('S1053811915002700', "pii")
#' }
object_retrieval <- function(
  id, # Identifier for abstract
  identifier = c("scopus_id", "eid", "doi", "pii", "pubmed_id"),
  ref = NULL,
  http_end = NULL, # any additional end to http statement.  See \code{\link{generic_elsevier_api}}
  ...
){

  identifier = match.arg(identifier)
  ender = paste0(paste(identifier, id, sep = "/"))
  if (!is.null(ref)){
    ender = paste(ender, "ref", ref, sep = "/")
  }
  if (!is.null(http_end)){
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
  #     warning("All arguments should be named in ..., may give unpredictable results")
  #   }
  #   if ("http_end" %in% n_args) {
  #     ender = paste(http_end, ender, sep = "/")
  #   }
  s = generic_elsevier_api( type = "object",
                            http_end = ender, ...)
  return(s)

}

