#' get_tags
#'
#' Lists all available tags. A public API connection can be used here.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#'
#' @return A dataframe containing the tags.
#' @export
#'
#' @examples
#' get_tags(api_client)
#'
get_tags <- function(api_client) {
  path = 'api/v1/tags/'

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  tags = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(tags)
}


#' get_tag_by_id
#'
#' Lists a tag by id. A public API connection can be used here.
#'
#' @param api_client The HttpClient returned by the connectToAPI or connectToPublicAPI functions.
#' @param id The tag's id.
#'
#' @return A dataframe containing the tag.
#' @export
#'
#' @examples
#' get_tag_by_id(api_client, '123')
#'
get_tag_by_id <- function(api_client, id) {
  path = qq('api/v1/tags/@{id}/')

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  tag = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(tag)
}
