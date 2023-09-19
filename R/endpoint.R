#' Endpoint
#'
#' @description
#' Endpoint object, storing HTTP client object and handling requests and
#'  responses to API
#'
Endpoint <- R6::R6Class(
  'Endpoint',
  public = list(
    #' @description
    #' Create an endpoint
    #'
    #' @param connection (crul::HttpClient) Connection object
    #'
    initialize = function (connection) {
      private$HttpClient = connection
    }
  ),

  private = list(
    #' @field STATUS_CODE_SUCCESS (list) Contains successful response codes
    STATUS_CODE_SUCCESS = list(200, 201),

    #' @field HttpClient (crul::HttpClient) Connection object
    HttpClient = NULL,

    #' @description
    #' Gets the full URL, formatted with any optional parameters
    #'
    #' @param entity_type (string) Type of entity to format URL
    #' @param endpoint (string) Endpoint to format URL
    #' @param ... (list) List of optional parameters
    #'
    #' @returns Formatted URL
    #'
    get_full_path = function (entity_type, endpoint, ...) {
      query = paste0(
        API_PATH_PREFIX,
        API_ENTITY_ENDPOINTS[[entity_type]][[endpoint]]
      )

      params = list(...)
      params = params[!is.na(params)]

      return (do.call(sprintf, c(query, params)))
    },

    #' @description
    #' Validates response against successful response codes, stops execution if
    #'  unsuccessful and outputs any response messages to console
    #'
    #' @param response (crul::HttpResponse) Response object
    #'
    #' @returns Returns response after validating
    #'
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

    #' @description
    #' Makes a request to provided url using the type specified, sends body and query
    #'  data when supplied
    #'
    #' @param type (string) Request type
    #' @param url (string) Request url
    #' @param body (list) Request body
    #' @param query (list) Query parameters
    #' @param encode (string) Encoding type
    #' @param as_df (bool) Whether to format as a dataframe
    #'
    #' @returns Response object
    #'
    make_request = function (type, url, body=NA, query=NA, encode='json', as_df=TRUE) {
      response = private$HttpClient[[type]](
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
