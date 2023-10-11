#' Templates
#'
#' @description
#' Templates object, inheriting from ConceptLibraryClient::Endpoint - allows
#'  querying of templates/ endpoints
#'
Templates <- R6::R6Class(
  'Templates',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries templates/
    #'
    #' @returns Response object
    #'
    get = function () {
      url = super$get_full_path('TEMPLATES', 'INDEX')
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries templates/{id}/get-versions/
    #'
    #' @param template_id (string) Id of entity to query
    #'
    #' @returns Response object
    #'
    get_versions = function (template_id) {
      url = super$get_full_path('TEMPLATES', 'VERSION_HISTORY', id=template_id)
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries templates/{id}/detail/ or templates/{id}/version/{id}/detail/
    #'
    #' @param template_id (string) Id of entity to query
    #' @param version_id (integer) Version id of entity to query
    #'
    #' @returns Response object
    #'
    get_detail = function (template_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'TEMPLATES', url, id=template_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
