#' get_tags
#'
#' Lists all available tags.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the tags.
#' @export
#'
#' @examples
#' get_tags()
#'
#' api_client = connect_to_API(public = FALSE)
#' get_tags(api_client = api_client)
#'
get_tags <- function(api_client = connect_to_API()) {
  # API call
  path = 'api/v1/tags/'
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  tags = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(tags)
}


#' get_tag_by_id
#'
#' Lists a tag by id.
#'
#' @param id The tag's id.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the tag.
#' @export
#'
#' @examples
#' get_tag_by_id('20')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_tag_by_id('20', api_client = api_client)
#'
get_tag_by_id <- function(id, api_client = connect_to_API()) {
  # API call
  path = qq('api/v1/tags/@{id}/')
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  tag = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(tag)
}
