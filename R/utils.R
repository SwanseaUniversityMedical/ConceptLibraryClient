#' clean_query_list
#'
#' Removes elements that are NA or FALSE from the list, and sets elements that are TRUE to 1.
#'
#' This function is used to create a cleaned query parameter list to pass into the query parameter of
#' crul::HTTPClient$get().
#'
#' @param query_list The query parameter list to clean.
#'
#' @return The cleaned list of query parameters.
#'
clean_query_list <- function(query_list) {
  cleaned_query_list = list()

  for (i in seq_along(query_list)) {
    param = query_list[[i]]
    if (!is.na(param[1])) {
      # Get string value of parameter name
      param_name = names(query_list)[i]

      if (length(param) > 1) {
        cleaned_query_list[[param_name]] = paste(param, collapse = ',')
      } else if (is.logical(param) && param == TRUE) {
        cleaned_query_list[[param_name]] = 1
      } else if (!is.logical(param)) {
        cleaned_query_list[[param_name]] = param
      }
    }
  }

  return(cleaned_query_list)
}

#' get_full_path
#'
#' Simple function which prepends api/v1 or api/v1/public to the beginning of a path, depending on whether the api
#' client's connection type (authenticated or public)
#'
#' @param path The path to prepend to.
#' @param api_client The api_client to determine connection type
#'
#' @return The full API path
#'
get_full_path <- function(path, api_client) {
  prefix = 'api/v1/public/'
  if (is_connection_authenticated(api_client)) {
    prefix = 'api/v1/'
  }
  return(paste0(prefix, path))
}


#' is_connection_public
#'
#' Determine whether the api connection is authenticated (TRUE) or public (FALSE).
#'
#' @param api_client The api connection to check.
#'
#' @return TRUE if authenticated, FALSE if public.
#'
is_connection_authenticated <- function(api_client) {
  return(length(api_client$auth) > 0)
}
