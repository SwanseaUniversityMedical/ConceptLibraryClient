#' get_concepts
#'
#' Lists all available concepts for the user.
#'
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#' @param search Search by part of concept name (do not put wild characters here)
#' @param tag_ids Specify vector of tags ids (get tags from get_tags())
#' @param show_only_my_concepts Only show concepts owned by me. Default is FALSE.
#' @param show_deleted_concepts Also show deleted concepts. Default is FALSE.
#' @param show_only_validated_concepts Show only validated concepts. Default is FALSE.
#' @param brand Show only concepts with a specified brand.
#' @param author Search by part of the author name.
#' @param owner_username Search by full username of the owner.
#' @param do_not_show_versions Do not show concepts versions. Default is FALSE (versions are shown).
#' @param must_have_published_versions Show only concepts which have a published version. Default is FALSE.
#'
#' @return A dataframe containing the concepts matching the query.
#' @export
#'
#' @examples
#' get_concepts()
#'
#' api_client = connect_to_API(public = FALSE)
#' get_concept(api_client = api_client)
#' get_concepts(
#'   api_client = api_client,
#'   search = 'Alcohol',
#'   tag_ids = c(11,4),
#'   show_only_my_concepts = TRUE,
#'   show_deleted_concepts = TRUE,
#'   show_only_validated_concepts = TRUE,
#'   brand = 'HDRUK',
#'   author = 'Kuan',
#'   owner_username = 'a.john',
#'   do_not_show_versions = TRUE,
#'   must_have_published_versions = TRUE)
#'
get_concepts <- function(
  api_client = connect_to_API(),
  search = NA,
  tag_ids = NA,
  show_only_my_concepts = FALSE,
  show_deleted_concepts = FALSE,
  show_only_validated_concepts = FALSE,
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
      show_only_my_concepts = show_only_my_concepts,
      show_deleted_concepts = show_deleted_concepts,
      show_only_validated_concepts = show_only_validated_concepts,
      brand = brand,
      author = author,
      owner_username = owner_username,
      do_not_show_versions = do_not_show_versions,
      must_have_published_versions = must_have_published_versions
    )
  } else {
    query_params = list(
      search = search,
      tag_ids = tag_ids,
      show_only_validated_concepts = show_only_validated_concepts,
      brand = brand,
      author = author,
      do_not_show_versions = do_not_show_versions
    )
  }

  # Clean query parameters to remove NA and FALSE values and change TRUE to 1
  cleaned_params = clean_query_list(query_params)

  # API call with path and query parameters
  path = get_full_path("concepts/", api_client)
  response = api_client$get(path = path, query = cleaned_params)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  concepts = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(concepts)
}

#' get_concept_by_id
#'
#' Lists a concept by id.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The concept's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the concept.
#' @export
#'
#' @examples
#' get_concept_by_id(api_client, 'C47')
#' get_concept_by_id(api_client, 'C47', use_public_api = TRUE)
#'
get_concept_by_id <- function(api_client, id, use_public_api = FALSE) {
  path = get_full_path(qq('concepts/@{id}/'), use_public_api)

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  concept = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(concept)
}

#' get_concept_detail
#'
#' Lists the concept detail of the latest version (or latest published version if using public API).
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The concept's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the concept detail.
#' @export
#'
#' @examples
#' get_concept_detail(api_client, 'C47')
#' get_concept_detail(api_client, 'C47', use_public_api = TRUE)
#'
get_concept_detail <- function(api_client, id, use_public_api = FALSE) {
  path = get_full_path(qq('concepts/@{id}/detail/'), use_public_api)

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  concept = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(concept)
}

#' get_concept_detail_by_version
#'
#' Lists the concept detail of the specified version
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The concept's id.
#' @param version_id The concept version's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the concept's detail.
#' @export
#'
#' @examples
#' get_concept_detail_by_version(api_client, 'C47', '54903')
#' get_concept_detail_by_version(api_client, 'C47', '24798', use_public_api = TRUE)
#'
get_concept_detail_by_version <- function(api_client, id, version_id, use_public_api = FALSE) {
  path = get_full_path(qq('concepts/@{id}/version/@{version_id}/detail/'), use_public_api)

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  concept = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(concept)
}

#' get_concept_code_list
#'
#' Exports the code list of the latest version of a concept for the user. This cannot be used with the public API.
#'
#' @param api_client The HttpClient returned by the connectToAPI function.
#' @param id The concept's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the code list.
#' @export
#'
#' @examples
#' get_concept_code_list(api_client, 'C47')
#'
get_concept_code_list <- function(api_client, id) {
  path = qq('api/v1/concepts/@{id}/export/codes/')

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  code_list = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(code_list)
}

#' get_concept_code_list_by_version
#'
#' Exports the code list of a specific version of a concept.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The concept's id.
#' @param version_id The concept version's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the code list.
#' @export
#'
#' @examples
#' get_concept_code_list_by_version(api_client, 'C47', '54903')
#' get_concept_code_list_by_version(api_client, 'C47', '24798', use_public_api = TRUE)
#'
get_concept_code_list_by_version <- function(api_client, id, version_id, use_public_api = FALSE) {
  path = get_full_path(qq('concepts/@{id}/version/@{version_id}/export/codes/'), use_public_api)

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  code_list = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(code_list)
}

#' get_concept_versions
#'
#' Lists all the versions of the concept.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The concept's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the concept versions.
#' @export
#'
#' @examples
#' get_concept_versions(api_client, 'C47')
#' get_concept_versions(api_client, 'C47', use_public_api = TRUE)
get_concept_versions <- function(api_client, id, use_public_api = FALSE) {
  path = get_full_path(qq('concepts/@{id}/get-versions/'), use_public_api)

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  versions = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  # In this case the data is contained as a list within the dataframe and needs to be accessed before returning.
  return(versions$versions[[1]])
}
