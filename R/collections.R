#' get_collections
#'
#' Lists all available collections.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the collections.
#' @export
#'
#' @examples
#' get_collections()
#'
#' api_client = connect_to_API(public = FALSE)
#' get_collections(api_client = api_client)
#'
get_collections <- function(api_client = connect_to_API()) {
  # API call
  path = 'api/v1/public/collections/'
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  collections = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(collections)
}


#' get_collection_by_id
#'
#' Lists a collection by id.
#'
#' @param id The collection's id.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the collection.
#' @export
#'
#' @examples
#' get_collection_by_id('20')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_collection_by_id('20', api_client = api_client)
#'
get_collection_by_id <- function(id, api_client = connect_to_API()) {
  # API call
  path = qq('api/v1/public/collections/@{id}/')
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  collection = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(collection)
}

#' exists_collection
#'
#' @param id
#' @param api_client
#'
#' @return
#'
exists_collection <- function (id, api_client) {
  path = qq('api/v1/public/collections/@{id}/');
  response = api_client$get(path = path);
  return (response$status_code == 200);
}
