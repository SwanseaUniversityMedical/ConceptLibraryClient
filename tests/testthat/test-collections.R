test_that("get_collections returns a non-empty dataframe with the authenticated API", {
  collections = get_collections(api_client = auth_client)

  expect_true(is.data.frame(collections))
  expect_true(nrow(collections) > 0)
})

test_that("get_collections returns a non-empty dataframe with the public API", {
  collections = get_collections(api_client = public_client)

  expect_true(is.data.frame(collections))
  expect_true(nrow(collections) > 0)
})

test_that("get_collections creates a public API connection when no connection is given", {
  collections = get_collections()

  expect_true(is.data.frame(collections))
  expect_true(nrow(collections) > 0)
})

test_that("get_collection_by_id returns a dataframe containing one row with the authenticated API", {
  collection = get_collection_by_id(20, api_client = auth_client)

  expect_true(is.data.frame(collection))
  expect_equal(nrow(collection), 1)
})

test_that("get_collection_by_id returns a dataframe containing one row with the public API", {
  collection = get_collection_by_id(20, api_client = public_client)

  expect_true(is.data.frame(collection))
  expect_equal(nrow(collection), 1)
})

test_that("get_collection_by_id creates a public API connection when no connection is given", {
  collection = get_collection_by_id(20)

  expect_true(is.data.frame(collection))
  expect_equal(nrow(collection), 1)
})
