Datasources <- R6::R6Class(
  'Datasources',
  inherit = Endpoint,
  public = list(
    #'
    get = function (...) {
      query_params = super$clean_query_params(...)

      url = super$get_full_path('DATASOURCES', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #'
    get_detail = function (datasource_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'DATASOURCES', url, id=datasource_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
