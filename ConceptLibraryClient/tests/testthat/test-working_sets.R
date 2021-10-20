test_that("get_working_sets returns a non-empty dataframe", {
  working_sets = get_working_sets(api_client)

  expect_true(is.data.frame(working_sets))
  expect_true(nrow(working_sets) > 0)
})

test_that("working sets can be filtered by search parameter", {
  search = "covid"
  working_sets = get_working_sets(api_client, search = search)

  expect_match(tolower(working_sets[1, "workingset_name"]), qq("^.*@{search}.*$"))
})


test_that("working sets can be filtered to see only those owned by the user", {
  skip("Can only be tested by user with owned working sets")
  working_sets = get_working_sets(api_client, show_only_my_workingsets = TRUE)

  expect_equal(working_sets[1,"owner"], config$username)
})

test_that("deleted working sets can be shown in the results", {
  skip("No deleted working sets in database to test on")
  working_sets = get_working_sets(api_client, show_deleted_workingsets = TRUE)

  expect_true("TRUE" %in% working_sets[,"is_deleted"])
})

test_that("working sets can be filtered by author", {
  author = "consign"
  working_sets = get_working_sets(api_client, author = author)

  expect_match(tolower(working_sets[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("working sets can be filtered by owner username", {
  owner = "rawlinga"
  working_sets = get_working_sets(api_client, owner_username = owner)

  expect_equal(working_sets[1, "owner"], owner)
})

test_that("working set versions can be hidden from results", {
  working_sets = get_working_sets(api_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(working_sets))
})

test_that("get_working_set_by_id returns a dataframe containing one row", {
  working_set = get_working_set_by_id(api_client, "WS1")

  expect_true(is.data.frame(working_set))
  expect_equal(nrow(working_set), 1)
})

test_that("get_working_set_detail returns a dataframe containing one row", {
  working_set = get_working_set_detail(api_client, "WS1")

  expect_true(is.data.frame(working_set))
  expect_equal(nrow(working_set), 1)
})

test_that("codes can be hidden from working set detail", {
  working_set = get_working_set_detail(api_client, "WS1", do_not_show_codes = TRUE)

  expect_false("codes" %in% names(working_set))
})

test_that("get_working_set_detail_by_version returns a dataframe containing one row", {
  working_set = get_working_set_detail_by_version(api_client, "WS1", "18")

  expect_true(is.data.frame(working_set))
  expect_equal(nrow(working_set), 1)
})

test_that("codes can be hidden from working set detail by version", {
  working_set = get_working_set_detail_by_version(api_client, "WS1", "18", do_not_show_codes = TRUE)

  expect_false("codes" %in% names(working_set))
})

test_that("get_working_set_code_list returns a non-empty dataframe", {
  code_list = get_working_set_code_list(api_client, "WS1")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_working_set_code_list_by_version returns a non-empty dataframe", {
  code_list = get_working_set_code_list_by_version(api_client, "WS1", "18")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_working_set_versions returns a non-empty dataframe", {
  versions = get_working_set_versions(api_client, "WS1")

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})
