################################################# get_working_sets #####################################################
########################################################################################################################

test_that("get_working_sets returns a non-empty dataframe", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_sets = get_working_sets(auth_client)

  expect_true(is.data.frame(working_sets))
  expect_true(nrow(working_sets) > 0)
})

test_that("working sets can be filtered by search parameter", {
#  skip("Skipping until re-implementation of workingsets is live")

  search = "covid"
  working_sets = get_working_sets(auth_client, search = search)

  expect_match(tolower(working_sets[1, "workingset_name"]), qq("^.*@{search}.*$"))
})

## Search by tags currently broken ##

test_that("working sets can be filtered to see only those owned by the user", {
#  skip("Skipping until re-implementation of workingsets is live")

  skip("Can only be tested by user with owned working sets")
  working_sets = get_working_sets(auth_client, show_only_my_workingsets = TRUE)

  expect_equal(working_sets[1,"owner"], config$username)
})

test_that("deleted working sets can be shown in the results", {
#  skip("Skipping until re-implementation of workingsets is live")

  skip("No deleted working sets in database to test on")
  working_sets = get_working_sets(auth_client, show_deleted_workingsets = TRUE)

  expect_true("TRUE" %in% working_sets[,"is_deleted"])
})

## Search by brand currently broken ##

test_that("working sets can be filtered by author", {
#  skip("Skipping until re-implementation of workingsets is live")

  author = "consign"
  working_sets = get_working_sets(auth_client, author = author)

  expect_match(tolower(working_sets[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("working sets can be filtered by owner username", {
#  skip("Skipping until re-implementation of workingsets is live")

  owner = "rawlinga"
  working_sets = get_working_sets(auth_client, owner_username = owner)

  expect_equal(working_sets[1, "owner"], owner)
})

test_that("working set versions can be hidden from results", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_sets = get_working_sets(auth_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(working_sets))
})

test_that("get_working_sets throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_sets(public_client), "Working Sets require an authenticated connection")
})

############################################## get_working_set_by_id ###################################################
########################################################################################################################

test_that("get_working_set_by_id returns a dataframe containing one row", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_set = get_working_set_by_id(auth_client, "WS1")

  expect_true(is.data.frame(working_set))
  expect_equal(nrow(working_set), 1)
})

test_that("get_working_set_by_id throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_set_by_id(public_client, "WS1"), "Working Sets require an authenticated connection")
})

############################################### get_working_set_detail #################################################
########################################################################################################################

test_that("get_working_set_detail returns a dataframe containing one row", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_set = get_working_set_detail(auth_client, "WS1")

  expect_true(is.data.frame(working_set))
  expect_equal(nrow(working_set), 1)
})

test_that("codes can be hidden from working set detail", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_set = get_working_set_detail(auth_client, "WS1", do_not_show_codes = TRUE)

  expect_false("codes" %in% names(working_set))
})

test_that("get_working_set_detail throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_set_detail(public_client, "WS1"), "Working Sets require an authenticated connection")
})

########################################## get_working_set_detail_by_version ###########################################
########################################################################################################################

test_that("get_working_set_detail_by_version returns a dataframe containing one row", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_set = get_working_set_detail_by_version(auth_client, "WS1", "18")

  expect_true(is.data.frame(working_set))
  expect_equal(nrow(working_set), 1)
})

test_that("codes can be hidden from working set detail by version", {
#  skip("Skipping until re-implementation of workingsets is live")

  working_set = get_working_set_detail_by_version(auth_client, "WS1", "18", do_not_show_codes = TRUE)

  expect_false("codes" %in% names(working_set))
})

test_that("get_working_set_detail_by_version throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_set_detail_by_version(public_client, "WS1", "18"), paste0("Working Sets require an ",
     "authenticated connection"))
})

########################################### get_working_set_code_list ##################################################
########################################################################################################################

test_that("get_working_set_code_list returns a non-empty dataframe", {
#  skip("Skipping until re-implementation of workingsets is live")

  code_list = get_working_set_code_list(auth_client, "WS1")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_working_set_code_list throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_set_code_list(public_client, "WS1"), "Working Sets require an authenticated connection")
})

####################################### get_working_set_code_list_by_version ###########################################
########################################################################################################################

test_that("get_working_set_code_list_by_version returns a non-empty dataframe", {
#  skip("Skipping until re-implementation of workingsets is live")

  code_list = get_working_set_code_list_by_version(auth_client, "WS1", "18")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_working_set_code_list_by_version throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_set_code_list_by_version(public_client, "WS1", "18"), paste0("Working Sets require an ",
      "authenticated connection"))
})

############################################ get_working_set_versions ##################################################
########################################################################################################################

test_that("get_working_set_versions returns a non-empty dataframe", {
#  skip("Skipping until re-implementation of workingsets is live")

  versions = get_working_set_versions(auth_client, "WS1")

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})

test_that("get_working_set_versions throws an error when used with the public API", {
#  skip("Skipping until re-implementation of workingsets is live")

  expect_error(get_working_set_versions(public_client, "WS1"), "Working Sets require an authenticated connection")
})
