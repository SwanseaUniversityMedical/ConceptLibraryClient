#' Collections
#'
#' @description
#' Collections object, inheriting from ConceptLibraryClient::Endpoint - allows
#'  querying of collections/ endpoints
#'
Collections <- R6::R6Class(
  'Collections',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries collections/
    #'
    #' @returns Response object
    #'
    get = function () {
      url = super$get_full_path('COLLECTIONS', 'INDEX')
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries collections/{id}/detail/
    #'
    #' @param collection_id (string) Id of entity to query
    #'
    #' @returns Response object
    #'
    get_detail = function (collection_id) {
      url = super$get_full_path(
        'COLLECTIONS', 'DETAIL', id=collection_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
