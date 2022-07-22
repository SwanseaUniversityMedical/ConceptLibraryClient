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

#' validate_type
#'
#' Simple function which validates input to expected data type.
#'
#' @param input The input data
#' @param data.type The expected data type of input
#'
#' @return TRUE/FALSE dependent on assertion
#'
validate_type <- function(input, data.type) {
  switch(data.type,
    string={
      result <- is.character(input) && length(input) == 1;
    },
    number={
      result <- is.numeric(input) && !is.na(input);
    },
    bool={
      result <- is.logical(input) && !is.na(input);
    }
  )

  return (result);
}

#' read_file
#'
#' Validates input and reads in contents of specified file.
#'
#' @param file.path Path including file name
#'
#' @return If input passes validation, returns contents of file,
#'         returns NA otherwise.
#'
read_file <- function(file.path) {
  if (validate_type(file.path, 'string')) {
    file.extension <- tail(strsplit(file.path, '\\.')[[1]], n=1)
    if (file.exists(file.path)) {
      if (tolower(file.extension) == 'yaml') {
        return (yaml::read_yaml(file.path));
      } else if (tolower(file.extension) == 'csv') {
        return (read.csv(file.path));
      }
    }
  }

  return (FALSE);
}

#' write_yaml_file
#'
#' Writes modified yaml data to file.
#'
#' @param phenotype.id Uploaded phenotype's id
#' @param concept.ids Uploaded phenotype's concept ids
#' @param file.path Path including file name
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function.
#'
write_yaml_file <- function (phenotype.id, concept.ids, file.path, api_client) {
  template_version <- API_YAML_TEMPLATE_VERSION;
  phenotype_version_id <- get_phenotype_detail(phenotype.id, api_client)$version_id
  concept_ids <- concept.ids;
  user <- strsplit(api_client$auth$userpwd, ':')[[1]][[1]]

  # Remove old data
  new.lines <- c();

  read.lines <- readLines(file.path);
  skip.lines <- 0;
  for (line.number in 1:length(read.lines)) {
    line.text <- trimws(tolower(read.lines[[line.number]]));

    if (skip.lines == 0) {
      for (field in API_TEMPLATE_FIELDS) {
        if (startsWith(line.text, field)) {
          skip.lines <- skip.lines + 1;

          if (startsWith(line.text, 'concept_ids')) {
            if (line.number + 1 <= length(read.lines)) {
              for (next.number in (line.number + 1):length(read.lines)) {
                next.line <- trimws(tolower(read.lines[[next.number]]));
                if (!startsWith(next.line, '-')) {
                  break;
                } else {
                  skip.lines <- skip.lines + 1;
                }
              }
            }
          }
        }
      }

      if (skip.lines == 0) {
        new.lines <- c(new.lines, read.lines[[line.number]]);
      } else {
        skip.lines <- skip.lines - 1;
      }
    } else {
      skip.lines <- skip.lines - 1;
    }
  }
  writeLines(new.lines, file.path);

  # Write new data
  write(qq('template_version: @{template_version}'), file=file.path, append=TRUE);
  write(qq('phenotype_id: @{phenotype.id}'), file=file.path, append=TRUE);
  write(qq('phenotype_version_id: @{phenotype_version_id}'), file=file.path, append=TRUE);

  write(qq('concept_ids:'), file=file.path, append=TRUE);
  for (concept.name in names(concept.ids)) {
    concept.id <- concept.ids[[concept.name]];
    write(qq('- @{concept.name}: @{concept.id}'), file=file.path, append=TRUE);
  }

  write(qq('user: @{user}'), file=file.path, append=TRUE);
}

#' validate_csv
#'
#' Validates csv file, including structure.
#'
#' @param file.path Path including file name
#'
#' @returns TRUE if valid file and structure, FALSE otherwise
#'
validate_csv <- function (file.path) {
  err = try(read_file(file.path))
  if (class(err) != 'try-error') {
    codes <- read_file(file.path);
    if (!validate_type(codes, 'bool')) {
      columns <- lapply(colnames(codes), function (x) tolower(x));
      if ("code" %in% columns) {
        return (TRUE);
      }
    }
  }
  return (FALSE);
}

#' validate_codes
#'
#' Validates list or string containing concept codelist.
#'
#' @param codes List or string containing codes.
#'
#' @returns TRUE if valid codelist, FALSE otherwise
#'
validate_codes <- function (codes) {
  if (is.list(codes) || (is.character(codes) && length(codes) != 1)) {
    codelist <- codes;
  } else if (validate_type(codes, 'string')) {
    codelist <- as.list(strsplit(trimws(codes), ',')[[1]]);
  }

  for (code in codelist) {
    if (!validate_type(code, 'string') && !validate_type(code, 'number')) {
      return (FALSE);
    }
  }
  return (TRUE);
}
