library(rvest)
library(dplyr)

#from http://r.789695.n4.nabble.com/r-release-r-oldrel-td4699220.html
## Split a version number to major, minor, patch
split_versions <- function(x) {
  x %>%
    sub(pattern = "^([0-9]+[.][0-9]+)$", replacement = "\\1.0") %>%
    strsplit(split = "[.]") %>%
    sapply(as.numeric)
}

## Sort version numbers
sort_tags <- function(x) {
  x_order <- x %>%
    split_versions() %>%
    apply(1, list) %>%
    lapply("[[", 1) %>%
    do.call(what = order)
  x [x_order]
}

# Get the HTML data
http = paste("http://www.elsevier.com/authors",
                 "author-schemas",
                 "elsevier-xml-dtds-and-transport-schemas", sep = "/")
html_data = read_html(http)
dtds = html_data %>% html_nodes("a")
# corr_dtds = html_data %>%
#   html_nodes("#content_container_22683 li a") %>%
#   html_text()
# corr_dtds = grep("sample file", corr_dtds, value = TRUE, invert = TRUE)
texts = dtds %>% html_text()
hrefs = dtds %>% html_attr("href")

df = data.frame(text = texts, href = hrefs,
                stringsAsFactors = FALSE)

keep = grepl("_dtd[.]txt$", trimws(tolower(df$href)))
df = df[keep, , drop = FALSE]

DTDS = c("JA", "SI", "EHS Book(s)", "Elsevier Book", "FLA")
DTDS = paste0("(", paste(DTDS, collapse = "|"), ") DTD")
df = df[ grep(DTDS, df$text), , drop = FALSE]
df$dtd = gsub("(.*) DTD (.*)", "\\1", df$text)
df$ver = gsub("(.*) DTD (.*)", "\\2", df$text)

df = arrange(df, dtd, ver)

end_df = df %>% group_by(dtd) %>% do(tail(., n = 1)) %>% as.data.frame

