Templates <- R6::R6Class(
  'Templates',
  inherit = Endpoint,
  public = list(
    #'
    get = function (...) {
      query_params = super$clean_query_params(...)

      url = super$get_full_path('TEMPLATES', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #'
    get_versions = function (template_id) {
      url = super$get_full_path('TEMPLATES', 'VERSION_HISTORY', id=template_id)
      return (super$make_request('get', url))
    },

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
