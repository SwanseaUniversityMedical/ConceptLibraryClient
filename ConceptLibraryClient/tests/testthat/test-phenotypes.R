test_that("get_phenotypes returns a non-empty dataframe", {
  phenotypes = get_phenotypes(api_client)

  expect_true(is.data.frame(phenotypes))
  expect_true(nrow(phenotypes) > 0)
})

test_that("get_published_phenotypes returns a non-empty dataframe", {
  skip_if(skip_public_API)

  phenotypes = get_published_phenotypes(public_api_client)

  expect_true(is.data.frame(phenotypes))
  expect_true(nrow(phenotypes) > 0)
})

test_that("phenotypes can be filtered by search parameter", {
  search = "alcohol"
  phenotypes = get_phenotypes(api_client, search = search)

  expect_match(tolower(phenotypes[1, "phenotype_name"]), qq("^.*@{search}.*$"))
})

test_that("published phenotypes can be filtered by search parameter", {
  skip_if(skip_public_API)

  search = "alcohol"
  phenotypes = get_published_phenotypes(public_api_client, search = search)

  expect_match(tolower(phenotypes[1, "phenotype_name"]), qq("^.*@{search}.*$"))
})

test_that("phenotypes can be filtered by tag", {
  tag_id = 18
  phenotypes = get_phenotypes(api_client, tag_ids = tag_id)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_id %in% tags)
})

test_that("published phenotypes can be filtered by tag", {
  skip_if(skip_public_API)

  tag_id = 18
  phenotypes = get_published_phenotypes(public_api_client, tag_ids = tag_id)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_id %in% tags)
})

test_that("phenotypes can be filtered by multiple tags", {
  tag_ids = c(18, 19)
  phenotypes = get_phenotypes(api_client, tag_ids = tag_ids)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_ids[1] %in% tags || tag_ids[2] %in% tags)
})

test_that("published phenotypes can be filtered by multiple tags", {
  skip_if(skip_public_API)

  tag_ids = c(18, 19)
  phenotypes = get_published_phenotypes(public_api_client, tag_ids = tag_ids)
  tags = phenotypes[[1, "tags"]][,"id"]

  expect_true(tag_ids[1] %in% tags|| tag_ids[2] %in% tagss)
})

test_that("phenotypes can be filtered to see only those owned by the user", {
  skip("Can only be tested by user with owned phenotypes")
  phenotypes = get_phenotypes(api_client, show_only_my_phenotypes = TRUE)

  expect_equal(phenotypes[1,"owner"], config$username)
})

# Can only be tested once there are deleted phenotypes in the database
test_that("deleted phenotypes can be shown in the results", {
  skip("No deleted phenotypes in database to test on")
  phenotypes = get_phenotypes(api_client, show_deleted_phenotypes = TRUE)

  expect_true("TRUE" %in% phenotypes[, "is_deleted"])
})

test_that("phenotypes can be filtered by author", {
  author = "carr"
  phenotypes = get_phenotypes(api_client, author = author)

  expect_match(tolower(phenotypes[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("published phenotypes can be filtered by author", {
  skip_if(skip_public_API)

  author = "carr"
  phenotypes = get_published_phenotypes(public_api_client, author = author)

  expect_match(tolower(phenotypes[1, "author"]), qq("^.*@{author}.*$"))
})

test_that("phenotypes can be filtered by owner username", {
  owner = "ieuan.scanlon"
  phenotypes = get_phenotypes(api_client, owner_username = owner)

  expect_equal(phenotypes[1, "owner"], owner)
})

test_that("phenotype versions can be hidden from results", {
  phenotypes = get_phenotypes(api_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(phenotypes))
})

test_that("published phenotype versions can be hidden from results", {
  skip_if(skip_public_API)

  phenotypes = get_published_phenotypes(public_api_client, do_not_show_versions = TRUE)

  expect_false("versions" %in% names(phenotypes))
})

test_that("phenotypes can be filtered to show only those with a published version", {
  phenotypes = get_phenotypes(api_client, must_have_published_versions = TRUE)

  expect_false("not published" %in% phenotypes[,"is_published"])
})

test_that("get_phenotype_by_id returns a dataframe containing one row", {
  phenotype = get_phenotype_by_id(api_client, "PH1")

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail returns a dataframe containing one row", {
  phenotype = get_phenotype_detail(api_client, "PH1")

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_detail_by_version returns a dataframe containing one row", {
  phenotype = get_phenotype_detail_by_version(api_client, "PH1", "2")

  expect_true(is.data.frame(phenotype))
  expect_true(nrow(phenotype) == 1)
})

test_that("get_phenotype_code_list returns a non-empty dataframe", {
  code_list = get_phenotype_code_list(api_client, "PH1", "2")

  expect_true(is.data.frame(code_list))
  expect_true(nrow(code_list) > 0)
})

test_that("get_phenotype_versions returns a non-empty dataframe", {
  versions = get_phenotype_versions(api_client, "PH1")

  expect_true(is.data.frame(versions))
  expect_true(nrow(versions) > 0)
})
