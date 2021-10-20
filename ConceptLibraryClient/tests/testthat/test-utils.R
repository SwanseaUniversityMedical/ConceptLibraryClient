test_that("clean_query_list returns a query list with a clean format for a URL", {
  query_list = list(
    vector = c(1,2,3),
    empty = NA,
    bool_true = TRUE,
    bool_false = FALSE,
    string = "string"
  )
  cleaned_params = clean_query_list(query_list)

  expect_equal(cleaned_params, list(vector = "1,2,3", bool_true = 1, string = "string"))
})
