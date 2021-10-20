test_that("get_concepts returns a non-empty dataframe", {
  concepts = get_concepts(api_client)

  expect_true(is.data.frame(concepts))
  expect_true(nrow(concepts) > 0)
})

test_that("get_published_concepts returns a non-empty dataframe", {
  skip_if(skip_public_API)

  concepts = get_published_concepts(public_api_client)

  expect_true(is.data.frame(concepts))
  expect_true(nrow(concepts) > 0)
})

test_that("concepts can be filtered by search parameter", {
  search = "alcohol"
  concepts = get_concepts(api_client, search = search)

  expect_match(tolower(concepts[1, "concept_name"]), qq("^.*@{search}.*$"))
})

test_that("published concepts can be filtered by search parameter", {
  skip_if(skip_public_API)

  search = "alcohol"
  concepts = get_published_concepts(public_api_client, search = search)

  expect_match(tolower(concepts[1, "concept_name"]), qq("^.*@{search}.*$"))
})

test_that("concepts can be filtered to see only those owned by the user", {
  skip("Can only be tested by user with owned concepts")
  concepts = get_concepts(api_client, show_only_my_concepts = TRUE)

  expect_equal(concepts[1,"owner"], config$username)
})

test_that("deleted concepts can be shown in the results", {
  concepts = get_concepts(api_client, show_deleted_concepts = TRUE)

  expect_true("TRUE" %in% concepts[,"is_deleted"])
})

test_that("concepts can be filtered by author", {
  author = "george"
  concepts = get_concepts(api_client, author = author)

  expect_match(tolower(concepts[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("published concepts can be filtered by author", {
  skip_if(skip_public_API)

  author = "george"
  concepts = get_published_concepts(public_api_client, author = author)

  expect_match(tolower(concepts[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("concepts can be filtered by owner username", {
  owner = "ieuan.scanlon"
  concepts = get_concepts(api_client, owner_username = owner)

  expect_equal(concepts[1, "owner"], owner)
})

test_that("concept versions can be hidden from results", {
  concepts = get_concepts(api_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(concepts))
})

test_that("published concept versions can be hidden from results", {
  skip_if(skip_public_API)

  concepts = get_published_concepts(public_api_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(concepts))
})

test_that("concepts can be filtered to show only those with a published version", {
  skip_if(skip_public_API)

  concepts = get_concepts(api_client, must_have_published_versions = TRUE)

  expect_false("not published" %in% concepts[,"is_published"])
})

test_that("get_concept_by_id returns a dataframe containing one row", {
  concept = get_concept_by_id(api_client, "C5")

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_detail returns a dataframe containing one row", {
  concept = get_concept_detail(api_client, "C5")

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_detail_by_version returns a dataframe containing one row", {
  concept = get_concept_detail(api_client, "C5", "1384")

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_code_list returns non-empty dataframe", {
  code_list = get_concept_code_list(api_client, "C5")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_concept_code_list_by_version returns a non-empty dataframe", {
  code_list = get_concept_code_list_by_version(api_client, "C5", "1384")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_concept_versions returns a non-empty dataframe", {
  versions = get_concept_versions(api_client, "C5")

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})
