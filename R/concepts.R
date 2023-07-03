Concepts <- R6::R6Class(
  'Concepts',
  inherit = Endpoint,
  public = list(
    #'
    get = function (...) {
      query_params = super$clean_query_params(...)

      url = super$get_full_path('CONCEPTS', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #'
    get_versions = function (phenotype_id) {
      url = super$get_full_path('CONCEPTS', 'VERSION_HISTORY', id=phenotype_id)
      return (super$make_request('get', url))
    },

    #'
    get_detail = function (phenotype_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'CONCEPTS', url, id=phenotype_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    },

    #'
    get_codelist = function (phenotype_id, version_id=NA) {
      url = if (is.na(version_id)) 'CODELIST' else 'CODELIST_BY_VERSION'
      url = super$get_full_path(
        'CONCEPTS', url, id=phenotype_id, version_id=version_id
      )

      return (super$make_request('get', url))
    }
  )
)
