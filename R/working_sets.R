#' check_working_set_connection
#'
#' Working Sets API require an authenticated connection. This function throws an error if a public connection is used.
#'
#' @param api_client The API client to check.
#'
check_working_set_connection <- function(api_client) {
  # Throw error if connection is not authenticated
  if (!is_connection_authenticated(api_client)) {
    stop("Working Sets require an authenticated connection. Use connect_to_API(public=FALSE) to create one")
  }
}

#' get_working_sets
#'
#' Lists all available working sets for the user. This cannot be used with the public API.
#'
#' @param api_client The HttpClient returned by the connectToAPI function. This must be an authenticated connection.
#' @param search Search by part of working set name (do not put wild characters here)
#' @param tag_ids Specify vector of tags ids (get tags from get_tags()). NOT CURRENTLY WORKING.
#' @param show_only_my_workingsets Only show working sets owned by me. Default is FALSE.
#' @param show_deleted_workingsets Also show deleted working sets. Default is FALSE.
#' @param brand Only show working sets with specified brand. NOT CURRENTLY WORKING.
#' @param author Search by part of the author name.
#' @param owner_username Search by full username of the owner.
#' @param do_not_show_versions Do not show working sets versions. Default is FALSE (versions are shown).
#'
#' @return A dataframe containing the working sets.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_sets(api_client)
#' get_working_sets(
#'   search = 'diab',
#'   tag_ids = c(11,4),                 # Excluded at the moment
#'   show_only_my_workingsets = TRUE,
#'   show_deleted_workingsets = TRUE,
#'   brand = 'ADP',                     # Excluded at the moment
#'   author = 'moh',
#'   owner_username = 'a.john',
#'   do_not_show_versions = TRUE)
#'
get_working_sets <- function(
  api_client,
  search = NA,
#  tag_ids = NA,
  show_only_my_workingsets = FALSE,
  show_deleted_workingsets = FALSE,
#  brand = NA,
  author = NA,
  owner_username = NA,
  do_not_show_versions = FALSE
) {
  check_working_set_connection(api_client)

  # Create list of named query parameters
  query_params = list(
    search = search,
  #  tag_ids = tag_ids,
    show_only_my_workingsets = show_only_my_workingsets,
    show_deleted_workingsets = show_deleted_workingsets,
  #  brand = brand,
    author = author,
    owner_username = owner_username,
    do_not_show_versions = do_not_show_versions
  )
  # Clean query parameters to remove NA and FALSE values and change TRUE to 1
  cleaned_params = clean_query_list(query_params)

  # API call with path and query parameters
  response = api_client$get(path = 'api/v1/workingsets/', query = cleaned_params)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  working_sets = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(working_sets)
}

#' get_working_set_by_id
#'
#' Lists a working set by id for the user. This cannot be used with the public API.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. This must be an authenticated
#' connection.
#' @param id The working set's id.
#'
#' @return A dataframe containing the working set.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_set_by_id(api_client, '123')
#'
get_working_set_by_id <- function(api_client, id) {
  check_working_set_connection(api_client)

  # API call
  path = qq('api/v1/workingsets/@{id}/')
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  working_set = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(working_set)
}

#' get_working_set_detail
#'
#' Lists the working set detail of the latest version. This cannot be used with the public API.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. This must be an authenticated
#' connection.
#' @param id The working set's id.
#' @param do_not_show_codes Do not show working set codes. Default is FALSE (codes are shown).
#'
#' @return A dataframe containing the working set detail.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_set_detail(api_client, '123')
#' get_working_set_detail(api_client, '123', do_not_show_codes = TRUE)
#'
get_working_set_detail <- function(api_client, id, do_not_show_codes = FALSE) {
  check_working_set_connection(api_client)

  query_list = clean_query_list(list(do_not_show_codes = do_not_show_codes))

  # API call
  path = qq('api/v1/workingsets/@{id}/detail/')
  response = api_client$get(path = path, query = query_list)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  working_set = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(working_set)
}

#' get_working_set_detail_by_version
#'
#' Lists the phenotype detail of the specified version. This cannot be used with the public API.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. This must be an authenticated
#' connection.
#' @param id The working set's id.
#' @param version_id The working set version's id.
#' @param do_not_show_codes Do not show working set codes. Default is FALSE (codes are shown).
#'
#' @return A dataframe containing the working set detail.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_set_detail_by_version(api_client, '123', '456')
#' get_working_set_detail_by_version(api_client, '123', '456', do_not_show_codes = TRUE)
#'
get_working_set_detail_by_version <- function(api_client, id, version_id, do_not_show_codes = FALSE) {
  check_working_set_connection(api_client)

  query_list = clean_query_list(list(do_not_show_codes = do_not_show_codes))

  # API call
  path = qq('api/v1/workingsets/@{id}/version/@{version_id}/detail/')
  response = api_client$get(path = path, query = query_list)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  working_set = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(working_set)
}

#' get_working_set_code_list
#'
#' Exports the code list of the latest version of a working set for the user. This cannot be used with the public
#' API.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. This must be an authenticated
#' connection.
#' @param id The working set's id.
#'
#' @return A dataframe containing the working set's code list.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_set_code_list(api_client, '123')
#'
get_working_set_code_list <- function(api_client, id) {
  check_working_set_connection(api_client)

  # API call
  path = qq('api/v1/workingsets/@{id}/export/codes/')
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  code_list = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(code_list)
}

#' get_working_set_code_list_by_version
#'
#' Exports the code list of a specific version of a working set for the user. This cannot be used with the public
#' API.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. This must be an authenticated
#' connection.
#' @param id The working set's id.
#' @param version_id The working set version's id.
#'
#' @return A dataframe containing the working set's code list.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_set_code_list_by_version(api_client, '123', '456')
#'
get_working_set_code_list_by_version <- function(api_client, id, version_id) {
  check_working_set_connection(api_client)

  # API call
  path = qq('api/v1/workingsets/@{id}/version/@{version_id}/export/codes/')
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  code_list = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(code_list)
}

#' get_working_set_versions
#'
#' Lists all the versions of the working set for the user. This cannot be used with the public API.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. This must be an authenticated
#' connection.
#' @param id The working set's id.
#'
#' @return A dataframe containing the working set's versions.
#' @export
#'
#' @examples
#' api_client = connect_to_API(public = FALSE)
#' get_working_set_versions(api_client, '123')
#'
get_working_set_versions <- function(api_client, id) {
  check_working_set_connection(api_client)

  # API call
  path = qq('api/v1/workingsets/@{id}/get-versions/')
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  versions = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  # In this case the data is contained as a list within the dataframe and needs to be accessed before returning.
  return(versions$versions[[1]])
}
