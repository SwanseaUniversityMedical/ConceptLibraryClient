#' get_data_sources
#'
#' Lists all data sources and the phenotypes associated with each.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param search Search by part of the data source name.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the data sources.
#' @export
#'
#' @examples
#' get_data_sources(api_client, search = 'hospital')
#' get_data_sources(api_client, use_public_api = TRUE)
#'
get_data_sources <- function(api_client, search = NA, use_public_api = FALSE) {
  path = get_full_path('data-sources/', use_public_api)

  # API call
  response = NA
  if (is.na(search)) {
    response = api_client$get(path = path)
  } else {
    response = api_client$get(path = path, query = list(search = search))
  }
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  data_sources = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(data_sources)
}

#' get_data_source_by_id
#'
#' Lists a data source by id and the associated phenotypes.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The data source's id.
#' @param use_public_api If the public API should be accessed instead. Default is FALSE.
#'
#' @return A dataframe containing the data source.
#' @export
#'
#' @examples
#' get_data_source_by_id(api_client, '425')
#' get_data_souce_by_id(api_client, '425', use_public_api = TRUE)
#'
get_data_source_by_id <- function(api_client, id, use_public_api = FALSE) {
  path = get_full_path(qq('data-sources/@{id}/'), use_public_api)

  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  data_source = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(data_source)
}
