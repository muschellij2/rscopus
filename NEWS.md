# rscopus 0.9.0

* Fixing `multi_author_retrieval` to actually work.

# rscopus 0.8.0

* Fixing the \link issues with packages.

# rscopus 0.7.0

* Fixed bug in bibtex core

# rscopus 0.6.6

* Deprecated `entries_to_df`, addresses #27.


# rscopus 0.6.5

* Added `wait_time` for scopus search for requested by @dkremmydas.
* Tried adding the ability to pass an Institution Token, but not sure if it worked.
* Added `plumx_metrics` function for API: https://dev.elsevier.com/documentation/PlumXMetricsAPI.wadl.


# rscopus 0.6.3

* Added `citation_retrieval` for example of the Citation Retrieval example.  
* Fixed output of functions.  Should look into `jsonlite::fromJSON(flatten = TRUE)` for some content.

# rscopus 0.6.0

* Added multiple author support using `multi_author_info` and `multi_author_retrieval`.  
* API Key now is hidden when printed.
* `read_cto` function and example CTO CSV are now included.
* `gen_entries_to_df` is now the default for most functions.
* Run `gsub` to replace identifier information such as `SCOPUS_ID:`.
* Added `scrub_identifer` to the `gen_entries_to_df`.

# rscopus 0.5.11

* `author_df` now has an option for `general` to use `gen_entries_to_df` instead of `entries_to_df`, which is more general.  
* `author_df_orig` is there for the old way of getting the data.frame

# rscopus 0.5.2

* Added a `NEWS.md` file to track changes to the package.

* Author retrieval is now able to pass a person's name instead of an ID always.


