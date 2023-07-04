#' Datasources
#'
#' @description
#' Datasources object, inheriting from ConceptLibraryClient::Endpoint - allows
#'  querying of data-sources/ endpoints
#'
Datasources <- R6::R6Class(
  'Datasources',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries data-sources/
    #'
    #' @return Response object
    #'
    get = function () {
      url = super$get_full_path('DATASOURCES', 'INDEX')
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries data-sources/{id}/detail/
    #'
    #' @params datasource_id (string) Id of entity to query
    #' @params version_id (integer) Version id of entity to query
    #'
    #' @return Response object
    #'
    get_detail = function (datasource_id) {
      url = super$get_full_path(
        'DATASOURCES', 'DETAIL', id=datasource_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
