#' get_phenotypes
#'
#' Lists all available phenotypes for the user and the data sources associate with each.
#'
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#' @param search Search by part of phenotype name (do not put wild characters here)
#' @param tag_ids Specify vector of tags ids (get tags from get_tags())
#' @param show_only_my_phenotypes Only show phenotypes owned by me. Default is FALSE.
#' @param show_deleted_phenotypes Also show deleted phenotypes. Default is FALSE.
#' @param show_only_validated_phenotypes Show only validated phenotypes. Default is FALSE.
#' @param brand Show only phenotypes with a specified brand.
#' @param author Search by part of the author name.
#' @param owner_username Search by full username of the owner.
#' @param do_not_show_versions Do not show phenotypes versions. Default is FALSE (versions are shown).
#' @param must_have_published_versions Show only phenotypes which have a published version. Default is FALSE.
#'
#' @return A dataframe containing the phenotypes matching the query.
#' @export
#'
#' @examples
#' get_phenotypes()
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotypes(api_client = api_client)
#' get_phenotypes(
#'   api_client = api_client,
#'   search = 'Alcohol',
#'   tag_ids = c(11,4),
#'   show_only_my_phenotypes = TRUE,
#'   show_deleted_phenotypes = TRUE,
#'   show_only_validated_phenotypes = TRUE,
#'   brand = 'HDRUK',
#'   author = 'Kuan',
#'   owner_username = 'a.john',
#'   do_not_show_versions = TRUE,
#'   must_have_published_versions = TRUE)
#'
get_phenotypes <- function(
  api_client = connect_to_API(),
  search = NA,
  tag_ids = NA,
  show_only_my_phenotypes = FALSE,
  show_deleted_phenotypes = FALSE,
  show_only_validated_phenotypes = FALSE,
  brand = NA,
  author = NA,
  owner_username = NA,
  do_not_show_versions = FALSE,
  must_have_published_versions = FALSE
) {
  # Create list of named query parameters
  query_params = list()
  if (is_connection_authenticated(api_client)) {
    query_params = list(
      search = search,
      tag_ids = tag_ids,
      show_only_my_phenotypes = show_only_my_phenotypes,
      show_deleted_phenotypes = show_deleted_phenotypes,
      show_only_validated_phenotypes = show_only_validated_phenotypes,
      brand = brand,
      author = author,
      owner_username = owner_username,
      do_not_show_versions = do_not_show_versions,
      must_have_published_versions =  must_have_published_versions
    )
  } else {
    query_params = list(
      search = search,
      tag_ids = tag_ids,
      show_only_validated_phenotypes = show_only_validated_phenotypes,
      brand = brand,
      author = author,
      do_not_show_versions = do_not_show_versions
    )
  }

  # Clean query parameters to remove NA and FALSE values and change TRUE to 1
  cleaned_params = clean_query_list(query_params)

  # API call with path and query parameters
  path = get_full_path('phenotypes/', api_client)
  response = api_client$get(path = path, query = cleaned_params)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotypes = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotypes)
}

#' get_phenotype_by_id
#'
#' Lists a phenotype by id and the data sources associated with it.
#'
#' @param id The phenotype's id.
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#'
#' @return A dataframe containing the phenotype.
#' @export
#'
#' @examples
#' get_phenotype_by_id('PH1')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_by_id('PH1', api_client = api_client)
#'
get_phenotype_by_id <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotype = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotype)
}

#' get_phenotype_detail
#'
#' Lists the phenotype detail of the latest version (or latest published version if using public API).
#'
#' @param id The phenotype's id.
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#'
#' @return A dataframe containing the phenotype detail.
#' @export
#'
#' @examples
#' get_phenotype_detail('PH1')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_detail('PH1', api_client = api_client)
#'
get_phenotype_detail <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/detail/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotype = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotype)
}

#' get_phenotype_detail_by_version
#'
#' Lists the phenotype detail of the specified version.
#'
#' @param id The phenotype's id.
#' @param version_id The phenotype version's id.
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#'
#' @return A dataframe containing the phenotype detail.
#' @export
#'
#' @examples
#' get_phenotype_detail_by_version('PH1', '2')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_detail_by_version('PH1', '2', api_client = api_client)
#'
get_phenotype_detail_by_version <- function(id, version_id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/version/@{version_id}/detail/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotype = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotype)
}

#' get_phenotype_code_list
#'
#' Exports the code list of a specific version of a phenotype.
#'
#' @param id The phenotype's id.
#' @param version_id The phenotype version's id.
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#'
#' @return A dataframe containing the code list.
#' @export
#'
#' @examples
#' get_phenotype_code_list('PH1', '2')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_code_list('PH1', '2', api_client = api_client)
#'
get_phenotype_code_list <- function(id, version_id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/version/@{version_id}/export/codes'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  code_list = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(code_list)
}

#' get_phenotype_versions
#'
#' Lists all the versions of the phenotype
#'
#' @param id The phenotype's id.
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#'
#' @return A dataframe containing the phenotype's versions.
#' @export
#'
#' @examples
#' get_phenotype_versions('PH1')
#'
#' api_client = connect_to_API()
#' get_phenotype_versions('PH1', api_client = api_client)
#'
get_phenotype_versions <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/get-versions/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  versions = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  # In this case the data is contained as a list within the dataframe and needs to be accessed before returning.
  return(versions$versions[[1]])
}
