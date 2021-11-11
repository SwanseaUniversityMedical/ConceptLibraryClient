test_that("calling get_phenotypes with the authenticated API returns a non-empty dataframe", {
  phenotypes = get_phenotypes(api_client = auth_client)

  expect_true(is.data.frame(phenotypes))
  expect_true(nrow(phenotypes) > 0)
})

test_that("calling get_phenotypes with the public API returns a non-empty dataframe", {
  phenotypes = get_phenotypes(api_client = public_client)

  expect_true(is.data.frame(phenotypes))
  expect_true(nrow(phenotypes) > 0)
})

test_that("calling get_phenotypes with no client object creates a public connection to return a non-empty dataframe", {
  phenotypes = get_phenotypes()

  expect_true(is.data.frame(phenotypes))
  expect_true(nrow(phenotypes) > 0)
})

test_that("an error is thrown when invalid parameters are given to get_phenotypes when using the public API", {
  expect_error(get_phenotypes(api_client = public_client, show_only_my_phenotypes = TRUE), paste0("One or more of the ",
              "parameters specified in get_phenotypes\\(\\) cannot be used with the public API"))
  expect_error(get_phenotypes(api_client = public_client, show_deleted_phenotypes = TRUE), paste0("One or more of the ",
              "parameters specified in get_phenotypes\\(\\) cannot be used with the public API"))
  expect_error(get_phenotypes(api_client = public_client, owner_username = "john.doe"), paste0("One or more of the ",
              "parameters specified in get_phenotypes\\(\\) cannot be used with the public API"))
  expect_error(get_phenotypes(api_client = public_client, must_have_published_versions = TRUE), paste0("One or more ",
              "of the parameters specified in get_phenotypes\\(\\) cannot be used with the public API"))
})

test_that("phenotypes can be filtered by search parameter with the authenticated API", {
  search = "alcohol"
  phenotypes = get_phenotypes(api_client = auth_client, search = search)

  expect_match(tolower(phenotypes[1, "phenotype_name"]), qq("^.*@{search}.*$"))
})

test_that("phenotypes can be filtered by search parameter with the public API", {
  search = "alcohol"
  phenotypes = get_phenotypes(api_client = public_client, search = search)

  expect_match(tolower(phenotypes[1, "phenotype_name"]), qq("^.*@{search}.*$"))
})

test_that("phenotypes can be filtered by tag with the authenticated API", {
  tag_id = 18
  phenotypes = get_phenotypes(api_client = auth_client, tag_ids = tag_id)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_id %in% tags)
})

test_that("phenotypes can be filtered by tag with the public API", {
  tag_id = 18
  phenotypes = get_phenotypes(api_client = public_client, tag_ids = tag_id)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_id %in% tags)
})

test_that("phenotypes can be filtered by multiple tags with the authenticated API", {
  tag_ids = c(18, 19)
  phenotypes = get_phenotypes(api_client = auth_client, tag_ids = tag_ids)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_ids[1] %in% tags || tag_ids[2] %in% tags)
})

test_that("phenotypes can be filtered by multiple tags with the public API", {
  tag_ids = c(18, 19)
  phenotypes = get_phenotypes(api_client = public_client, tag_ids = tag_ids)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_ids[1] %in% tags|| tag_ids[2] %in% tagss)
})

# Unskip if user owns phenotype
test_that("phenotypes can be filtered to see only those owned by the user with the authenticated API", {
  skip("Can only be tested by user with owned phenotypes")
  phenotypes = get_phenotypes(api_client = auth_client, show_only_my_phenotypes = TRUE)

  expect_equal(phenotypes[1,"owner"], auth_client$auth$user)
})

# Can only be tested once there are deleted phenotypes in the database
test_that("deleted phenotypes can be shown in the results with the authenticated API", {
  skip("No deleted phenotypes in database to test on")
  phenotypes = get_phenotypes(api_client = auth_client, show_deleted_phenotypes = TRUE)

  expect_true("TRUE" %in% phenotypes[, "is_deleted"])
})

# Can only be tested once there are validated phenotypes in the database
test_that("phenotypes can be filtered to only show validated phenotypes with the authenticated API", {
  skip("No validated phenotypes in database to test on")

  phenotypes = get_phenotypes(api_client = auth_client, show_only_validated_phenotypes = TRUE)
  phenotype_id = phenotypes[1, "phenotype_id"]
  phenotype_detail = get_phenotype_detail(phenotype_id, api_client = auth_client)

  expect_equal(phenotype_detail[1, "validation_performed"], "True")
})

test_that("phenotypes can be filtered to only show validated phenotypes with the public API", {
  skip("No validated phenotypes in database to test on")

  phenotypes = get_phenotypes(api_client = public_client, show_only_validated_phenotypes = TRUE)
  phenotyp_id = phenotypes[1, "phenotype_id"]
  phenotype_detail = get_phenotype_detail(phenotype_id, api_client = public_client)

  expect_equal(phenotype_detail[1, "validation_performed"], "True")
})

test_that("phenotypes can be filtered by brand with the authenticated API", {
  phenotypes = get_phenotypes(api_client = auth_client, brand = "hdruk")

  # Brand not currently returned by API, just check that response is not empty for now
  expect_true(nrow(phenotypes) > 0)
})

test_that("phenotypes can be filtered by brand with the public API", {
  phenotypes = get_phenotypes(api_client = public_client, brand = "hdruk")

  # Brand not currently returned by API, just check that response is not empty for now
  expect_true(nrow(phenotypes) > 0)
})

test_that("phenotypes can be filtered by author with the authenticated API", {
  author = "carr"
  phenotypes = get_phenotypes(api_client = auth_client, author = author)

  expect_match(tolower(phenotypes[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("phenotypes can be filtered by author with the public API", {
  author = "carr"
  phenotypes = get_phenotypes(api_client = public_client, author = author)

  expect_match(tolower(phenotypes[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("phenotypes can be filtered by owner username with the authenticated API", {
  owner = "ieuan.scanlon"
  phenotypes = get_phenotypes(api_client = auth_client, owner_username = owner)

  expect_equal(phenotypes[1, "owner"], owner)
})

test_that("phenotype versions can be hidden from results with the authenticated API", {
  phenotypes = get_phenotypes(api_client = auth_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(phenotypes))
})

test_that("phenotype versions can be hidden from results with the public API", {
  phenotypes = get_phenotypes(api_client = public_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(phenotypes))
})

test_that("phenotypes can be filtered to show only those with a published version with the authenticated API", {
  phenotypes = get_phenotypes(api_client = auth_client, must_have_published_versions = TRUE)

  expect_false("not published" %in% phenotypes[,"is_published"])
})

test_that("get_phenotype_by_id returns a dataframe containing one row with the authenticated API", {
  phenotype = get_phenotype_by_id("PH1", api_client = auth_client)

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_by_id returns a dataframe containing one row with the public API", {
  phenotype = get_phenotype_by_id("PH1", api_client = public_client)

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_by_id creates a public API connection when no connection is given", {
  phenotype = get_phenotype_by_id("PH1")

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail returns a dataframe containing one row with the authenticated API", {
  phenotype = get_phenotype_detail("PH1", api_client = auth_client)

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail returns a dataframe containing one row with the public API", {
  phenotype = get_phenotype_detail("PH1", api_client = public_client)

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail creates a public API connection when no connection is given", {
  phenotype = get_phenotype_detail("PH1")

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail_by_version returns a dataframe containing one row with the authenticated API", {
  phenotype = get_phenotype_detail_by_version("PH1", "2", api_client = auth_client)

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail_by_version returns a dataframe containing one row with the public API", {
  phenotype = get_phenotype_detail_by_version("PH1", "2", api_client = public_client)

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail_by_version creates a public API connection when no connection is given", {
  phenotype = get_phenotype_detail_by_version("PH1", "2")

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_code_list returns a non-empty dataframe with the authenticated API", {
  code_list = get_phenotype_code_list("PH1", "2", api_client = auth_client)

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_phenotype_code_list returns a non-empty dataframe with the public API", {
  code_list = get_phenotype_code_list("PH1", "2", api_client = public_client)

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_phenotype_code_list creates a public API connection when no connection is given", {
  code_list = get_phenotype_code_list("PH1", "2")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_phenotype_versions returns a non-empty dataframe with the authenticate API", {
  versions = get_phenotype_versions("PH1", api_client = auth_client)

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})

test_that("get_phenotype_versions returns a non-empty dataframe with the public API", {
  versions = get_phenotype_versions("PH1", api_client = public_client)

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})

test_that("get_phenotype_versions creates a public API connection when no connection is given", {
  versions = get_phenotype_versions("PH1")

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})
