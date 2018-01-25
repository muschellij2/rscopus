ensure_names = function(x, cn) {
  sd = c(setdiff(names(x), cn))
  for (isd in sd) {
    x[[isd]] = NA
  }
  sd = setdiff(cn, names(x))
  for (isd in sd) {
    x[[isd]] = NA
  }
  return(x)
}

bind_list = function(L) {
  L = lapply(L, function(x) {
    if (!is.data.frame(x)) {
      x = unlist(x)
    }
    x
  })
  cn = sapply(L, names)
  cn = unique(c(unlist(cn)))
  L = lapply(L, function(x){
    x = ensure_names(x, cn)
    x[cn]
  })
  L = do.call("rbind", L)
  if (!is.data.frame(L)) {
    L = as.data.frame(L, stringsAsFactors = FALSE)
  }
  return(L)
}

