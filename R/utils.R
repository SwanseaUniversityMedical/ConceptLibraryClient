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
      if (tolower(file.extension) %in% YAML_FILE_EXTENSIONS) {
        return (yaml::read_yaml(file.path));
      } else if (tolower(file.extension) == 'csv') {
        return (read.csv(file.path, check.names=FALSE));
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
write_yaml_file <- function (yaml_data, phenotype.id, concept.ids, file.path, api_client) {
  replacement.fields <- list(
    template_version = API_YAML_TEMPLATE_VERSION,
    phenotype_id = phenotype.id,
    phenotype_version_id = get_phenotype_detail(phenotype.id, api_client)$version_id,
    user = strsplit(api_client$auth$userpwd, ':')[[1]][[1]]
  )
  for (field in API_TEMPLATE_FIELDS) {
    if (is.null(yaml_data[[field]])) {
      new.field <- list();
      new.field[[field]] <- replacement.fields[[field]];

      yaml_data <- c(yaml_data, new.field);
    } else {
      yaml_data[[field]] <- replacement.fields[[field]];
    }
  }

  concept.version.ids <- list()
  for (concept.name in names(concept.ids)) {
    concept.version.ids[[concept.name]] <- get_concept_detail(concept.ids[[concept.name]], api_client)$version_id
  }

  concept.update <- list()
  for (c.idx in 1:length(yaml_data$concepts)) {
    concept.name <- names(yaml_data$concepts[[c.idx]]);
    concept.update[[concept.name]] <- list(version.id = FALSE, concept.id = FALSE);

    for (f.idx in 1:length(yaml_data$concepts[[c.idx]][[concept.name]])) {
      field.name <- names(yaml_data$concepts[[c.idx]][[concept.name]][[f.idx]]);

      if (field.name == 'concept_version_id') {
        yaml_data$concepts[[c.idx]][[concept.name]][[f.idx]][[field.name]] = concept.version.ids[[concept.name]];
        concept.update[[concept.name]]$version.id = TRUE;
      }

      if (field.name == 'concept_id') {
        concept.update[[concept.name]]$concept.id = TRUE;
      }
    }

    if (!concept.update[[concept.name]]$concept.id) {
      yaml_data$concepts[[c.idx]][[concept.name]][[length(yaml_data$concepts[[c.idx]][[concept.name]]) + 1]] <- list(
        concept_id = concept.ids[[concept.name]]
      );
    }

    if (!concept.update[[concept.name]]$version.id) {
      yaml_data$concepts[[c.idx]][[concept.name]][[length(yaml_data$concepts[[c.idx]][[concept.name]]) + 1]] <- list(
        concept_version_id = concept.version.ids[[concept.name]]
      );
    }
  }

  yaml::write_yaml(yaml_data, file.path);
}

#' validate_csv
#'
#' Validates csv file, including structure.
#'
#' @param file.path Path including file name
#' @param def.code
#'
#' @returns TRUE if valid file and structure, FALSE otherwise
#'
validate_csv <- function (file.path, def.code=NA, def.descr=NA) {
  err = try(read_file(file.path))
  if (class(err) != 'try-error') {
    codes <- read_file(file.path);
    if (!validate_type(codes, 'bool')) {
      code.column <- NA
      if (!is.na(def.code)) {
        if (validate_type(def.code, 'string') && def.code %in% names(codes)) {
          code.column <- which(colnames(codes)==def.code)[1]
        } else {
          return (FALSE);
        }
      } else {
        code.column <- which(grepl("(?i)code", names(codes))==TRUE)[1]
      }

      if (!is.na(def.descr)) {
        if (!validate_type(def.descr, 'string') || !(def.descr %in% names(codes))) {
          return (FALSE);
        }
      }

      if (!is.na(code.column)) {
        # Remove rows with missing code column
        codes <- codes[!(is.na(codes[,code.column]) | codes[,code.column] == ''),]

        return (nrow(codes) >= 1);
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

#' should_write_field
#'
#' Determines whether the field has content that should be written
#'
#' @param value
#'
#' @returns TRUE if content, FALSE otherwise
#'
should_write_field <- function (value) {
  return (
    !is.null(value) && !is.na(value) && value != ''
  )
}
