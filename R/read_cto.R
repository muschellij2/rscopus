#' Read Citation Overview (CTO) File
#'
#' @param file CSV of CTO export from Scopus
#'
#' @return A list of the data, year columns, and
#' header information
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' file = system.file("extdata", "CTOExport.csv", package = "rscopus")
#' citations_over_time = read_cto(file)
#' citations_over_time = citations_over_time$data
read_cto = function(file) {
  x = readLines(file)
  # there is a header
  ss = strsplit(x, split = ",")
  start = which(
    sapply(ss, function(x) {
      all(x %in% "")
    }))
  if (length(start) > 0) {
    start = start[length(start)]

    ind = seq(1, start)
    hdr_info = ss[ind]
    hdr_info = sapply(hdr_info,
                      paste,
                      collapse = "")
    hdr_info = hdr_info[ hdr_info != "" ]
    hdr_info = gsub('"', "", hdr_info)
    hdr_info = trimws(hdr_info)
    x = x[-ind]
    ss = ss[-ind]
  } else {
    hdr_info = NULL
  }

  # header is a bit weird
  hdr = ss[1:2]
  yrs = hdr[[1]]
  hdr = hdr[[2]]
  replacers = yrs != ""
  yrs = yrs[replacers]

  hdr[ replacers]  = yrs
  ss = ss[3:length(ss)]
  ss = sapply(ss, function(x) {
    paste(x, collapse = ",")
  })
  tfile = tempfile(fileext = ".csv")
  writeLines(ss, con = tfile)
  res = utils::read.csv(tfile, header = FALSE, as.is = TRUE)
  colnames(res) = hdr
  L = list(data = res,
           year_columns = yrs)
  L$author_information = hdr_info
  return(L)
}

#' @rdname read_cto
#' @importFrom tidyr gather_
#' @export
read_cto_long = function(file) {
  res = read_cto(file)
  df = res$data
  yrs = res$year_columns
  removers = c("subtotal", "total")
  cn = colnames(df)
  yrs = yrs[ !(yrs %in% removers)]
  cn = cn[ !(cn %in% removers)]
  df = df[, cn]
  long = tidyr::gather_(
    data = df, key_col = "year",
    value_col = "citations" ,
    gather_cols = yrs)
  long$year = factor(long$year,
                     levels = yrs)
  L = list(data = long,
           year_columns = yrs)
  L$author_information = res$hdr_info
  return(L)
}