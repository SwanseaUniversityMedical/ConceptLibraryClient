# Search term used to make tests run faster by reducing API response content.
search = 'alcohol'

################################################## get_concepts ########################################################
########################################################################################################################

test_that("calling get_concepts with the authenticated API returns a non-empty dataframe", {
  concepts = get_concepts(api_client = auth_client, search = search)

  expect_true(is.data.frame(concepts))
  expect_true(nrow(concepts) > 0)
})

test_that("calling get_concepts with the public API returns a non-empty dataframe", {
  concepts = get_concepts(api_client = public_client, search = search)

  expect_true(is.data.frame(concepts))
  expect_true(nrow(concepts) > 0)
})

test_that("calling get_concepts with no client object creates a public connection to return a non-empty dataframe", {
  concepts = get_concepts(search = search)

  expect_true(is.data.frame(concepts))
  expect_true(nrow(concepts) > 0)
})

test_that("an error is thrown when invalid parameters are given to get_concepts when using the public API", {
  expect_error(get_concepts(api_client = public_client, show_only_my_concepts = TRUE), paste0("One or more of the ",
              "parameters specified in get_concepts\\(\\) cannot be used with the public API"))
  expect_error(get_concepts(api_client = public_client, show_deleted_concepts = TRUE), paste0("One or more of the ",
              "parameters specified in get_concepts\\(\\) cannot be used with the public API"))
  expect_error(get_concepts(api_client = public_client, owner_username = "john.doe"), paste0("One or more of the ",
              "parameters specified in get_concepts\\(\\) cannot be used with the public API"))
  expect_error(get_concepts(api_client = public_client, must_have_published_versions = TRUE), paste0("One or more ",
              "of the parameters specified in get_concepts\\(\\) cannot be used with the public API"))
})

test_that("concepts can be filtered by search parameter with the authenticated API", {
  concepts = get_concepts(api_client = auth_client, search = search)

  expect_match(tolower(concepts[1, "concept_name"]), qq("^.*@{search}.*$"))
})

test_that("concepts can be filtered by search parameter with the public API", {
  concepts = get_concepts(api_client = public_client, search = search)

  expect_match(tolower(concepts[1, "concept_name"]), qq("^.*@{search}.*$"))
})

test_that("concepts can be filtered by tag with the authenticated API", {
  skip("Tag filter broken atm")
  tag_id = 20
  concepts = get_concepts(api_client = auth_client, tag_ids = tag_id)
  tags = concepts[[1, "tags"]][,"id"]

  expect_true(tag_id %in% tags)
})

test_that("concepts can be filtered by tag with the public API", {
  skip("Tag filter broken atm")
  tag_id = 20
  concepts = get_concepts(api_client = public_client, tag_ids = tag_id)
  tags = concepts[[1, "tags"]][,"id"]

  expect_true(tag_id %in% tags)
})

test_that("concepts can be filtered by multiple tags with the authenticated API", {
  skip("Tag filter broken atm")
  tag_ids = c(19, 20)
  concepts = get_concepts(api_client = auth_client, tag_ids = tag_ids)
  tags = concepts[[1, "tags"]][,"id"]

  expect_true(tag_ids[1] %in% tags || tag_ids[2] %in% tags)
})

test_that("concepts can be filtered by multiple tags with the public API", {
  skip("Tag filter broken atm")
  tag_ids = c(19, 20)
  concepts = get_concepts(api_client = public_client, tag_ids = tag_ids)
  tags = concepts[[1, "tags"]][,"id"]

  expect_true(tag_ids[1] %in% tags|| tag_ids[2] %in% tagss)
})

test_that("concepts can be filtered to see only those owned by the user with the authenticated API", {
  skip("Can only be tested by user with owned concepts")
  concepts = get_concepts(api_client = auth_client, show_only_my_concepts = TRUE)

  expect_equal(concepts[1,"owner"], auth_client$auth$user)
})

test_that("deleted concepts can be shown in the results with the authenticated API", {
  # Use search term which returns deleted concepts
  search_deleted = 'covid-19'
  concepts = get_concepts(api_client = auth_client, search = search_deleted, show_deleted_concepts = TRUE)

  expect_true("TRUE" %in% concepts[,"is_deleted"])
})

test_that("concepts can be filtered to only show validated concepts with the authenticated API", {
  concepts = get_concepts(api_client = auth_client, search = search, show_only_validated_concepts = TRUE)
  concept_id = concepts[1, "concept_id"]
  concept_detail = get_concept_detail(concept_id, api_client = auth_client)

  expect_true(concept_detail[1, "validation_performed"])
})

test_that("concepts can be filtered to only show validated concepts with the public API", {
  concepts = get_concepts(api_client = public_client, search = search, show_only_validated_concepts = TRUE)
  concept_id = concepts[1, "concept_id"]
  concept_detail = get_concept_detail(concept_id, api_client = public_client)

  expect_true(concept_detail[1, "validation_performed"])
})

test_that("concepts can be filtered by brand with the authenticated API", {
  concepts = get_concepts(api_client = auth_client, search = search, brand = "hdruk")

  # Brand not currently returned by API, just check that response is not empty for now
  expect_true(nrow(concepts) > 0)
})

test_that("concepts can be filtered by brand with the public API", {
  concepts = get_concepts(api_client = public_client, search = search, brand = "hdruk")

  # Brand not currently returned by API, just check that response is not empty for now
  expect_true(nrow(concepts) > 0)
})

test_that("concepts can be filtered by author with the authenticated API", {
  author = "george"
  concepts = get_concepts(api_client = auth_client, author = author)

  expect_match(tolower(concepts[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("concepts can be filtered by author with the public API", {
  author = "george"
  concepts = get_concepts(api_client = public_client, author = author)

  expect_match(tolower(concepts[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("concepts can be filtered by owner username with the authenticated API", {
  owner = "ieuan.scanlon"
  concepts = get_concepts(api_client = auth_client, owner_username = owner)

  expect_equal(concepts[1, "owner"], owner)
})

test_that("concept versions can be hidden from results with the authenticated API", {
  concepts = get_concepts(api_client = auth_client, search = search, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(concepts))
})

test_that("concept versions can be hidden from results with the public API", {
  concepts = get_concepts(api_client = public_client, search = search, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(concepts))
})

test_that("concepts can be filtered to show only those with a published version with the authenticated API", {
  concepts = get_concepts(api_client = auth_client, search = search, must_have_published_versions = TRUE)

  expect_false("not published" %in% concepts[,"is_published"])
})

############################################### get_concept_by_id ######################################################
########################################################################################################################

test_that("get_concept_by_id returns a dataframe containing one row with the authenticated API", {
  concept = get_concept_by_id("C6594", api_client = auth_client)

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_by_id returns a dataframe containing one row with the public API", {
  concept = get_concept_by_id("C6594", api_client = public_client)

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_by_id creates a public API connection when no connection is given", {
  concept = get_concept_by_id("C6594")

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

################################################ get_concept_detail ####################################################
########################################################################################################################

test_that("get_concept_detail returns a dataframe containing one row with the authenticated API", {
  concept = get_concept_detail("C6594", api_client = auth_client)

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_detail returns a dataframe containing one row with the public API", {
  concept = get_concept_detail("C6594", api_client = public_client)

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_detail creates a public API connection when no connection is given", {
  concept = get_concept_detail("C6594")

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

########################################### get_concept_detail_by_version ##############################################
########################################################################################################################

test_that("get_concept_detail_by_version returns a dataframe containing one row with the authenticated API", {
  concept = get_concept_detail_by_version("C6594", "47428", api_client = auth_client)

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_detail_by_version returns a dataframe containing one row with the public API", {
  concept = get_concept_detail_by_version("C6594", "47428", api_client = public_client)

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

test_that("get_concept_detail_by_version creates a public API connection when no connection is given", {
  concept = get_concept_detail_by_version("C6594", "47428")

  expect_true(is.data.frame(concept))
  expect_true(nrow(concept) == 1)
})

###############################################  get_concept_code_list #################################################
########################################################################################################################

test_that("get_concept_code_list returns non-empty dataframe with the authenticated API", {
  code_list = get_concept_code_list("C6594", auth_client)

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_concept_code_list throws an error when used with the public API", {
  expect_error(get_concept_code_list("C6594", public_client), "requires an authenticated connection")
})

######################################### get_concept_code_list_by_version #############################################
########################################################################################################################

test_that("get_concept_code_list_by_version returns a non-empty dataframe with the authenticated API", {
  code_list = get_concept_code_list_by_version("C6594", "47428", api_client = auth_client)

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_concept_code_list_by_version returns a non-empty dataframe with the public API", {
  code_list = get_concept_code_list_by_version("C6594", "47428", api_client = public_client)

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_concept_code_list_by_version creates a public API connection when no connection is given", {
  code_list = get_concept_code_list_by_version("C6594", "47428")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

################################################ get_concept_versions ##################################################
########################################################################################################################

test_that("get_concept_versions returns a non-empty dataframe with the authenticated API", {
  versions = get_concept_versions("C6594", api_client = auth_client)

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})

test_that("get_concept_versions returns a non-empty dataframe with the public API", {
  versions = get_concept_versions("C6594", api_client = public_client)

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})

test_that("get_concept_versions creates a public API connection when no connection is given", {
  versions = get_concept_versions("C6594")

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})
