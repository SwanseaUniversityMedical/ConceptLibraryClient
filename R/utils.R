#' set_gateway_proxy
#'
#' @description
#' Sets the user system environment variable to the gateway proxy.
#' This method is only called when the user is utilising the DOMAINS$GATEWAY URL
#'
#' @returns NA
#'
set_gateway_proxy <- function () {
  Sys.setenv(http_proxy='http://proxy:8080')
  Sys.setenv(https_proxy='http://proxy:8080')
}

#' validate_type
#'
#' @description
#' Validates input against a type; string, number, bool
#'
#' @param input (any) Input to be compared
#' @param type (string) Type in {'string', 'number', 'bool'}
#'
#' @returns TRUE if type matches, FALSE otherwise
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
#' @returns Data from the file if validation succeeds, NULL otherwise
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
#' @returns TRUE if empty, FALSE otherwise
#'
is_empty = function (input) {
  return (all(is.null(input)) || all(is.na(input)) || all(input == ''))
}

#' try_parse_doi
#'
#' @description
#' Tries to parse all DOIs from list of publications
#'
#' @param publications (list) List of publications from entity definition file
#'
#' @returns List containing detail and doi
try_parse_doi = function (publications) {
  output = list()
  for (publication in publications) {
    if (!validate_type(publication, 'string') || trimws(publication) == '') {
      next
    }

    pattern = regexpr(
      '\\b(10[.][0-9]{4,}(?:[.][0-9]+)*\\/(?:(?![\\"&\'<>])\\S)+)\\b',
      publication,
      perl=TRUE
    )

    match = regmatches(publication, pattern)

    output = append(output, list(list(
      details = publication,
      doi = if (length(match) == 0) NA else match
    )))
  }

  return (output)
}
