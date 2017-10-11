context("Reading in Author Information")


expect_pass = function(expr) {
  # testthat::expect_that(object, !testthat::gives_warning())
  testthat::expect_failure(expect_warning(expr))
  testthat::expect_failure(expect_error(expr))
}


test_that("author_df_works", {

  if (have_api_key()) {
    expect_pass({
        dd = author_df(au_id = "8858259000", all_author_info = TRUE)
    })
  }

  if (have_api_key()) {
    expect_pass({
      dd = author_df(au_id = "8858259000", all_author_info = FALSE)
    })
  }
})
