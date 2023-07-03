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

#'
is_empty = function (value) {
  return (all(is.null(value)) || all(is.na(value)) || all(value == ''))
}
