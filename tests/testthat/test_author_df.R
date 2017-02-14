context("Reading in Author Information")

test_that("author_df_works", {

  if (have_api_key()) {
    expect_success({
      dd = author_df(au_id = "8858259000", all_author_info = TRUE)
    })
  }

  if (have_api_key()) {
    expect_success({
      dd = author_df(au_id = "8858259000", all_author_info = FALSE)
    })
  }
})
