test_that("get_tags returns a non-empty dataframe", {
  tags = get_tags(api_client)

  expect_true(is.data.frame(tags))
  expect_true(nrow(tags) > 0)
})

test_that("get_tag_by_id returns a dataframe containing one row", {
  tag = get_tag_by_id(api_client, 20)

  expect_true(is.data.frame(tag))
  expect_equal(nrow(tag), 1)
})
