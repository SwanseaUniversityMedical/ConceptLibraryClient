Endpoint <- R6::R6Class(
  'Endpoint',
  public = list(
    #' @field HttpClient (crul::HttpClient) a
    HttpClient = NULL,

    #'
    initialize = function (connection) {
      self$HttpClient = connection
    }
  ),

  private = list(
    #'
    STATUS_CODE_SUCCESS = c(200, 201),

    #' @description a
    get_full_path = function (entity_type, endpoint, ...) {
      query = paste0(
        API_PATH_PREFIX,
        API_ENTITY_ENDPOINTS[[entity_type]][[endpoint]]
      )

      params = list(...)
      params = params[!is.na(params)]

      return (do.call(sprintf, c(query, params)))
    },

    #' @description a
    clean_query_params = function (...) {
      params = list(...)

      return (params)
    },

    #' @description a
    check_response = function (response) {
      if (!(response$status_code %in% private$STATUS_CODE_SUCCESS)) {
        stop(jsonlite::prettify(response$parse('utf-8')))
      }

      response = jsonlite::fromJSON(response$parse('utf-8'))
      if ('message' %in% response) {
        message(jsonlite::prettify(response))
      }

      return (response)
    },

    #' @description a
    make_request = function (type, url, body=NA, query=NA, encode='json', as_df=TRUE) {
      response = self$HttpClient[[type]](
        path=url,
        query=query,
        body=body,
        encode=encode
      )

      response = private$check_response(response)
      response = if (as_df) data.frame(response) else response

      return (response)
    }
  )
)
