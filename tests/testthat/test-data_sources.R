test_that("get_data_sources returns a non-empty dataframe", {
  data_sources = get_data_sources(api_client)

  expect_true(is.data.frame(data_sources))
  expect_true(nrow(data_sources) > 0)
})

test_that("data sources can be filtered by search parameter", {
  search = "hospital"
  data_sources = get_data_sources(api_client, search = search)

  expect_match(tolower(data_sources[1, "name"]), qq("^.*@{search}.*$"))
})

test_that("get_data_source_by_id returns a dataframe containing one row", {
  data_source = get_data_source_by_id(api_client, "26")

  expect_true(is.data.frame(data_source))
  expect_equal(nrow(data_source), 1)
})
