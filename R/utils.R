#' validate_type
#'
#' @description
#' Validates input against a type; string, number, bool
#'
#' @param input (any) Input to be compared
#' @param type (string) Type in {'string', 'number', 'bool'}
#'
#' @return TRUE if type matches, FALSE otherwise
#'
validate_type <- function (input, type) {
  switch(type,
    string={
      result = is.character(input) && length(input) == 1
    },
    number={
      result = is.numeric(input) && !is.na(input)
    },
    bool={
      result = is.logical(input) && !is.na(input)
    }
  )

  return (result)
}

#' read_file
#'
#' @description
#' Reads in a file from path, using given function if the file exists
#'  and matches optional extensions list
#'
#' @param path (string) Path to read the file in from
#' @param fun (function) Function used to read in the file
#' @param extensions (list) List of valid extensions
#'
#' @return Data from the file if validation succeeds, NULL otherwise
#'
read_file <- function (path, fun, extensions=NA) {
  if (!validate_type(path, 'string')) {
    return (NULL)
  }

  if (!anyNA(extensions)) {
    file_extension = tolower(tail(strsplit(path, '\\.')[[1]], n=1))
    if (!(file_extension %in% extensions)) {
      return (NULL)
    }
  }

  if (file.exists(path)) {
    return (fun(path))
  }

  return (NULL)
}

#' is_empty
#'
#' @description
#' Checks whether given input is empty
#'
#' @param input (any) Input to be compared
#'
#' @return TRUE if empty, FALSE otherwise
#'
is_empty = function (input) {
  return (all(is.null(input)) || all(is.na(input)) || all(input == ''))
}
