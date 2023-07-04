#' Concepts
#'
#' @description
#' Concepts object, inheriting from ConceptLibraryClient::Endpoint - allows
#'  querying of concepts/ endpoints
#'
Concepts <- R6::R6Class(
  'Concepts',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries concepts/, with optional query parameters
    #'
    #' @params ... (list) List of optional parameters
    #'
    #' @return Response object
    #'
    get = function (...) {
      query_params = list(...)

      url = super$get_full_path('CONCEPTS', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #' @description
    #' Queries concepts/{id}/get-versions/
    #'
    #' @params concept_id (string) Id of entity to query
    #'
    #' @return Response object
    #'
    get_versions = function (concept_id) {
      url = super$get_full_path('CONCEPTS', 'VERSION_HISTORY', id=concept_id)
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries concepts/{id}/detail/ or concepts/{id}/version/{id}/detail/
    #'
    #' @params concept_id (string) Id of entity to query
    #' @params version_id (integer) Version id of entity to query
    #'
    #' @return Response object
    #'
    get_detail = function (concept_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'CONCEPTS', url, id=concept_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    },

    #' @description
    #' Queries concepts/{id}/export/codes/ or
    #'  concepts/{id}/version/{id}/export/codes/
    #'
    #' @params concept_id (string) Id of entity to query
    #' @params version_id (integer) Version id of entity to query
    #'
    #' @return Response object
    #'
    get_codelist = function (concept_id, version_id=NA) {
      url = if (is.na(version_id)) 'CODELIST' else 'CODELIST_BY_VERSION'
      url = super$get_full_path(
        'CONCEPTS', url, id=concept_id, version_id=version_id
      )

      return (super$make_request('get', url))
    }
  )
)
