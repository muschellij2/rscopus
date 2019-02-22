#' Retrieve PlumX metrics for Scopus documents and other related artifacts
#'
#' @param value The value of the identifier to search for.
#' @param type The type of identifier to search for.
#' @param ... Additional arguments to pass to \code{\link{generic_elsevier_api}},
#' other than \code{http_end}, \code{type}, \code{search_type}, and
#' \code{content_type}
#'
#' @note See \url{https://dev.elsevier.com/documentation/PlumXMetricsAPI.wadl}
#' for more information
#'
#' @return List of elements, content and the \code{GET} request
#' @export
#'
#' @examples
#' if (have_api_key()) {
#' type = "doi"
#' value = "10.1016/j.nicl.2018.10.013"
#' res = plumx_metrics(value, type)
#' }
plumx_metrics = function(
  value,
  type = plumx_types(),
  ...
){
  type = match.arg(type)
  http_end = paste0("/", type, "/", value)
  res = generic_elsevier_api(
    http_end = http_end,
    type = "analytics",
    search_type = "plumx",
    content_type = NA,
    ...
  )
  return(res)
}

#' @export
#' @rdname plumx_metrics
plumx_types = function() {
  c("airitiDocId",
    "arxivId",
    "cabiAbstractId",
    "citeulikeId",
    "digitalMeasuresArtifactId",
    "doi",
    "elsevierId",
    "elsevierPii",
    "facebookCountUrlId",
    "figshareArticleId",
    "githubRepoId",
    "isbn",
    "lccn",
    "medwaveId",
    "nctId",
    "oclc",
    "pittEprintDscholarId",
    "pmcid",
    "pmid",
    "redditId",
    "repecHandle",
    "repoUrl",
    "scieloId",
    "sdEid",
    "slideshareUrlId",
    "smithsonianPddrId",
    "soundcloudTrackId",
    "ssrnId",
    "urlId",
    "usPatentApplicationId",
    "usPatentPublicationId",
    "vimeoVideoId",
    "youtubeVideoId")
}