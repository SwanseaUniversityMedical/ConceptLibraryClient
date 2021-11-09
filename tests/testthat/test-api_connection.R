# The connection functions were called in the setup script, just accessing the HttpClient objects here
test_that("connect_to_API can create an authenticated API connection", {
  expect_true(length(auth_client$auth) > 0)
})

test_that("connect_to__API can create a public API connection", {
  expect_true(length(public_client$auth) == 0)
})

test_that("connect_to_API can create an authenticated API connection to a different URL", {
  skip("Skipped to avoid multiple login boxes. Unskip at least once when running tests.")
  client = connect_to_API(url = demo_site_url, public = FALSE)

  expect_equal(client$url, demo_site_url)
  expect_true(length(client$auth) > 0)
})

test_that("connect_to_API can create a public API connection to a different URL", {
  client = connect_to_API(url=demo_site_url)

  expect_equal(client$url, demo_site_url)
  expect_true(length(client$auth) == 0)
})

test_that("An error is returned when the API response is not 200.", {
  expect_error(get_phenotype_by_id(api_client, "null"))
})
