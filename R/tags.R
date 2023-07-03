Tags <- R6::R6Class(
  'Tags',
  inherit = Endpoint,
  public = list(
    #'
    get = function (...) {
      query_params = super$clean_query_params(...)

      url = super$get_full_path('TAGS', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #'
    get_detail = function (tag_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'TAGS', url, id=tag_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    }
  )
)
