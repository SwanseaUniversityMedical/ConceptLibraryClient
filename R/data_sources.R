#' get_data_sources
#'
#' Lists all data sources and the phenotypes associated with each.
#'
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#' @param search Search by part of the data source name.
#'
#' @return A dataframe containing the data sources.
#' @export
#'
#' @examples
#' get_data_sources()
#'
#' api_client = connect_to_API(public = FALSE)
#' get_data_sources(api_client = api_client, search = 'hospital')
#'
get_data_sources <- function(api_client = connect_to_API(), search = NA) {
  # API call
  path = get_full_path('data-sources/', api_client)
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
#' @param id The data source's id.
#' @param api_client The HttpClient returned by the connect_to_API function. Optional, a public API connection is
#' created if left blank.
#'
#' @return A dataframe containing the data source.
#' @export
#'
#' @examples
#' get_data_source_by_id('26')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_data_souce_by_id('26', api_client = api_client)
#'
get_data_source_by_id <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('data-sources/@{id}/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  data_source = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(data_source)
}
