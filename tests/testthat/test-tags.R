test_that("get_tags returns a non-empty dataframe with the authenticated API", {
  tags = get_tags(api_client = auth_client)

  expect_true(is.data.frame(tags))
  expect_true(nrow(tags) > 0)
})

test_that("get_tags returns a non-empty dataframe with the public API", {
  tags = get_tags(api_client = public_client)

  expect_true(is.data.frame(tags))
  expect_true(nrow(tags) > 0)
})

test_that("get_tags creates a public API connection when no connection is given", {
  tags = get_tags()

  expect_true(is.data.frame(tags))
  expect_true(nrow(tags) > 0)
})

test_that("get_tag_by_id returns a dataframe containing one row with the authenticated API", {
  tag = get_tag_by_id(4, api_client = auth_client)

  expect_true(is.data.frame(tag))
  expect_equal(nrow(tag), 1)
})

test_that("get_tag_by_id returns a dataframe containing one row with the public API", {
  tag = get_tag_by_id(4, api_client = public_client)

  expect_true(is.data.frame(tag))
  expect_equal(nrow(tag), 1)
})

test_that("get_tag_by_id creates a public API connection when no connection is given", {
  tag = get_tag_by_id(4)

  expect_true(is.data.frame(tag))
  expect_equal(nrow(tag), 1)
})
