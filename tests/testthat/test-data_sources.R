# Search term used to make tests run faster by reducing API response content.
search = 'university'

################################################ get_data_sources ######################################################
########################################################################################################################

test_that("get_data_sources returns a non-empty dataframe with the authenticated API", {
  data_sources = get_data_sources(api_client = auth_client, search = search)

  expect_true(is.data.frame(data_sources))
  expect_true(nrow(data_sources) > 0)
})

test_that("get_data_sources returns a non-empty dataframe with the public API", {
  data_sources = get_data_sources(api_client = public_client, search = search)

  expect_true(is.data.frame(data_sources))
  expect_true(nrow(data_sources) > 0)
})

test_that("get_data_sources creates a public API connection when no connection is given", {
  data_sources = get_data_sources(search = search)

  expect_true(is.data.frame(data_sources))
  expect_true(nrow(data_sources) > 0)
})

test_that("data sources can be filtered by search parameter with the authenticated API", {
  data_sources = get_data_sources(api_client = auth_client, search = search)

  expect_match(tolower(data_sources[1, "name"]), qq("^.*@{search}.*$"))
})

test_that("data sources can be filtered by search parameter with the public API", {
  data_sources = get_data_sources(api_client = public_client, search = search)

  expect_match(tolower(data_sources[1, "name"]), qq("^.*@{search}.*$"))
})

############################################## get_data_source_by_id ###################################################
########################################################################################################################

test_that("get_data_source_by_id returns a dataframe containing one row with the authenticated API", {
  data_source = get_data_source_by_id("3", api_client = auth_client)

  expect_true(is.data.frame(data_source))
  expect_equal(nrow(data_source), 1)
})

test_that("get_data_source_by_id returns a dataframe containing one row with the public API", {
  data_source = get_data_source_by_id("3", api_client = public_client)

  expect_true(is.data.frame(data_source))
  expect_equal(nrow(data_source), 1)
})

test_that("get_data_source_by_id creates a public API connection when no connection is given", {
  data_source = get_data_source_by_id("3")

  expect_true(is.data.frame(data_source))
  expect_equal(nrow(data_source), 1)
})
