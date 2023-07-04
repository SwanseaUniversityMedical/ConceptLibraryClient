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
    #' @return Response object
    #'
    get = function () {
      url = super$get_full_path('COLLECTIONS', 'INDEX')
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries collections/{id}/detail/
    #'
    #' @params collection_id (string) Id of entity to query
    #' @params version_id (integer) Version id of entity to query
    #'
    #' @return Response object
    #'
    get_detail = function (collection_id) {
      url = super$get_full_path(
        'COLLECTIONS', 'DETAIL', id=collection_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
