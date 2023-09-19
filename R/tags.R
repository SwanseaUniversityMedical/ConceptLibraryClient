#' Tags
#'
#' @description
#' Tags object, inheriting from ConceptLibraryClient::Endpoint - allows
#'  querying of tags/ endpoints
#'
Tags <- R6::R6Class(
  'Tags',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries tags/
    #'
    #' @returns Response object
    #'
    get = function () {
      url = super$get_full_path('TAGS', 'INDEX')
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries tags/{id}/detail/
    #'
    #' @param tag_id (string) Id of entity to query
    #'
    #' @returns Response object
    #'
    get_detail = function (tag_id) {
      url = super$get_full_path(
        'TAGS', 'DETAIL', id=tag_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
