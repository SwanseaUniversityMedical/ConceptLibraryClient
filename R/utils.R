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
#' Simple function which prepends api/ or api/public to the beginning of a path, depending on whether the public api
#' should be used.
#'
#' @param path The path to prepend to.
#' @param use_public_api If the public API should be used instead.
#'
#' @return The full API path
#'
get_full_path <- function(path, use_public_api) {
  prefix = 'api/v1/'
  if (use_public_api == TRUE) {
    prefix = 'api/v1/public/'
  }

  return(paste0(prefix, path))
}
