# The connection functions were called in the setup script, just accessing the HttpClient objects here
test_that("connect_to_API creates an authenticated API connection", {
  expect_true(length(api_client$auth) > 0)
})

test_that("connect_to_public_API creates an unauthenticated API connection", {
  expect_true(length(public_api_client$auth) == 0)
})

test_that("An error is returned when the API response is not 200.", {
  expect_error(get_phenotype_by_id(api_client, "null"))
})
