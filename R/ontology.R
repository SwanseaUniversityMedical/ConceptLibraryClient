#' Ontology
#'
#' @description
#' Ontology object, inheriting from ConceptLibraryClient::Endpoint - allows
#' querying of ontology/ endpoints
#'
Ontology <- R6::R6Class(
  'Ontology',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries ontology/node/, with optional query parameters
    #'
    #' @param ... (list) List of optional parameters
    #'
    #' @returns Response object
    #'
    get = function (...) {
      query_params = list(...)

      url = super$get_full_path('ONTOLOGY', 'SEARCH')
      return (super$make_request('get', url, query=query_params))
    },

    #' @description
    #' Queries ontology/node/{id}/
    #'
    #' @param node_id (string) Id of entity to query
    #'
    #' @returns Response object
    #'
    get_detail = function (node_id) {
      url = super$get_full_path(
        'ONTOLOGY', 'DETAIL', id=node_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    },

    #' @description
    #' Queries ontology/
    #'
    #' @returns Response object
    #'
    get_groups = function () {
      url = super$get_full_path('ONTOLOGY', 'GROUPS')

      return (super$make_request('get', url, as_df=FALSE))
    },

    #' @description
    #' Queries ontology/type/{id}/
    #'
    #' @param ontology_id (string) Id of entity to query
    #'
    #' @returns Response object
    #'
    get_group_detail = function (ontology_id) {
      url = super$get_full_path(
        'ONTOLOGY', 'TYPE', id=ontology_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
