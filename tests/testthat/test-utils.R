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

test_that("get_full_path prepends the correct prefix when the connection is authenticated", {
  path = get_full_path('test/', auth_client)

  expect_equal(path, 'api/v1/test/')
})

test_that("get_full_path prepends the correct prefix when the connection is public", {
  path = get_full_path('test/', public_client)

  expect_equal(path, 'api/v1/public/test/')
})

test_that("is_connection_authenticated correctly detects the connection type", {
  expect_true(is_connection_authenticated(auth_client))
  expect_false(is_connection_authenticated(public_client))
})
