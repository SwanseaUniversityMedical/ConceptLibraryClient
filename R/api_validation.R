#' api_validate_phenotype_concepts
#'
#' Validates phenotype's concept data
#'
#' @param data Phenotype and concept data read from yaml file
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function.
#' @param is.valid Bool, used to pass on whether previous checks were invalid or not
#'
#' @return TRUE if valid, FALSE otherwise
#'
api_validate_phenotype_concepts <- function (data, api_client, is.update, is.valid) {
  concepts <- data$concepts;

  if (is.list(concepts)) {
    for (concept in data$concepts) {
      concept_name <- names(concept);

      if (is.null(concept_name) || !validate_type(concept_name, 'string')) {
        warning('Validation error: Concept names must be strings');
        is.valid <- FALSE;
      } else {
        params <- do.call(list, unlist(concept[[1]], recursive=FALSE));

        for (field in concept[[1]]) {
          key <- names(field);
          if (is.null(key) || !validate_type(key, 'string')) {
            warning('Validation error: Concept keys must be strings');
            is.valid <- FALSE;
            break;
          }
        }

        # Field: type
        if (!is.null(params$type) && validate_type(params$type, 'string')) {
          if (!(trimws(params$type) %in% API_PHENOTYPE_VALIDATION$CONCEPTS)) {
            warning('Validation error: Concept \'type\' not in valid range (csv, existing_concept, inline)');
            is.valid <- FALSE;
          } else {
            # Field: coding_system
            if (params$type %in% list('csv', 'inline')) {
              if (!is.null(params$coding_system) && validate_type(params$coding_system, 'number')) {
                if (!(params$coding_system %in% names(API_PHENOTYPE_VALIDATION$CODING_SYSTEM))) {
                  warning('Validation error: Concept \'coding_system\' not in valid range, see documentation');
                  is.valid <- FALSE;
                }
              } else {
                warning('Validation error: Concept \'coding_system\'is missing or invalid type (int)');
                is.valid <- FALSE;
              }
            }

            # Check required fields for type
            if (params$type == 'csv') {
              if (!is.null(params$filepath) && validate_type(params$filepath, 'string')) {
                code.column <- NA
                if (!is.null(params$code_column)) {
                  code.column <- params$code_column
                }

                descr.column <- NA
                if (!is.null(params$description_column)) {
                  descr.column <- params$description_column
                }

                is.valid.file <- validate_csv(
                  params$filepath,
                  def.code=code.column,
                  def.descr=descr.column
                );
                if (!is.valid.file) {
                  warning(paste0(
                    'Validation error: Concept \'filepath\' is invalid (file:',
                    params$filepath,
                    '), file type or structure (\'code\' column must be present,
                    defined code/description column may be missing, no empty or invalid column names), see example'
                  ));
                  is.valid <- FALSE;
                }
              } else {
                warning('Validation error: Concept \'filepath\'is missing or invalid type (string) for concept of type \'csv\'');
                is.valid <- FALSE;
              }
            } else if (params$type == 'existing_concept') {
              if (!is.null(params$concept_id) && validate_type(params$concept_id, 'string')) {
                concept.exists <- exists_concept(params$concept_id, api_client);
                if (!concept.exists) {
                  warning('Validation error: Concept \'concept_id\' does not exist');
                  is.valid <- FALSE;
                }
              } else {
                warning('Validation error: Concept \'concept_id\'is missing or invalid type (string) for concept of type \'existing_concept\'');
                is.valid <- FALSE;
              }
            } else if (params$type == 'inline') {
              if (!is.null(params$codes)) {
                is.valid.codes <- validate_codes(params$codes);
                if (!is.valid.codes) {
                  warning('Validation error: Concept \'codes\' codelist is invalid, see example');
                  is.valid <- FALSE;
                }
              } else {
                warning('Validation error: Concept \'codes\'is missing for concept of type \'inline\'');
                is.valid <- FALSE;
              }
            }
          }
        } else {
          warning('Validation error: Concept \'type\'is missing or invalid type (string)');
          is.valid <- FALSE;
        }

        # Check IDs for update
        if (is.update) {
          if (!is.null(params$concept_id) && params$type != 'existing_concept') {
            if (startsWith(params$concept_id, 'C')) {
              phenotype.id <- as.numeric(strsplit(params$concept_id, 'C')[[1]][[-1]]);
              if (!validate_type(phenotype.id, 'number')) {
                warning('Validation error: Concept \'concept_id\' is incorrectly formatted');
                is.valid <- FALSE;
              } else {
                concept.exists <- exists_concept(params$concept_id, api_client);
                if (!concept.exists) {
                  warning('Validation error: Concept \'concept_id\' does not exist');
                  is.valid <- FALSE;
                }
              }
            }
          }
        }
      }
    }
  } else {
    warning('Validation error: \'concepts\' does not follow valid format (nested list), see example');
    is.valid <- FALSE;
  }

  return (is.valid);
}

#' api_validate_phenotype_datasources
#'
#' Validates datasources stored in the phenotype's data
#'
#' @param data Phenotype and concept data read from yaml file
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function.
#' @param is.valid Bool, used to pass on whether previous checks were invalid or not
#'
#' @return TRUE if valid, FALSE otherwise
#'
api_validate_phenotype_datasources <- function (data, api_client, is.valid) {
  datasources <- data$data_sources;
  if (!is.null(datasources)) {
    if (is.vector(datasources)) {
      for (datasource.id in datasources) {
        if (validate_type(datasource.id, 'number')) {
          if (!exists_data_source(datasource.id, api_client)) {
            warning('Validation error: \'data_sources\' source id does not exist');
            is.valid <- FALSE;
          }
        } else {
          warning('Validation error: \'data_sources\' list element is incorrect type (int)');
          is.valid <- FALSE;
        }
      }
    } else if (validate_type(datasources, 'number')) {
      if (!exists_data_source(datasource.id, api_client)) {
        warning('Validation error: \'data_sources\' source id does not exist');
        is.valid <- FALSE;
      }
    } else {
      warning('Validation error: \'data_sources\' is incorrect type (int)');
      is.valid <- FALSE;
    }
  }

  return (is.valid);
}

#' api_validate_phenotype
#'
#' Validates phenotype data and then validates concept and datasources
#'
#' @param data Phenotype and concept data read from yaml file
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function.
#' @param is.update Whether the phenotype and concepts are being updated
#'
#' @return TRUE if valid, FALSE otherwise
#'
api_validate_phenotype <- function (data, api_client, is.update) {
  is.valid <- TRUE;

  # Check ID for update
  if (is.update) {
    if (!is.null(data$phenotype_id)) {
      if (startsWith(data$phenotype_id, 'PH')) {
        phenotype.id <- as.numeric(strsplit(data$phenotype_id, 'PH')[[1]][[-1]]);
        if (!validate_type(phenotype.id, 'number')) {
          warning('Validation error: \'phenotype_id\' is incorrectly formatted');
          is.valid <- FALSE;
        } else {
          phenotype.exists <- exists_phenotype(data$phenotype_id, api_client);
          if (!phenotype.exists) {
            warning('Validation error: \'phenotype_id\' does not exist');
            is.valid <- FALSE;
          }
        }
      } else {
        warning('Validation error: \'phenotype_id\' is incorrectly formatted')
        is.valid <- FALSE;
      }
    }
  }

  # Field: publish
  if (!is.null(data$publish) && !validate_type(data$publish, 'bool')) {
    warning('Validation error: \'publish\' is incorrect type (bool)');
    is.valid <- FALSE;
  }

  # Field: title
  if (is.null(data$title) || !validate_type(data$title, 'string')) {
    warning('Validation error: \'title\' is missing or incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: author
  if (!is.null(data$author)) {
    is.valid.author <- lapply(data$author, function (x) { x <- !validate_type(x, 'string') });
    if (any(as.logical(is.valid.author))) {
      warning('Validation error: \'author\' is incorrect type (string or list<string>)');
      is.valid <- FALSE;
    }
  } else {
    warning('Validation error: \'author\' is missing');
    is.valid <- FALSE;
  }

  # Field: sex
  if (!is.null(data$sex)) {
    is.valid.sex <- lapply(data$sex, function (x) { x <- !validate_type(x, 'string') });
    if (any(as.logical(is.valid.sex))) {
      warning('Validation error: \'sex\' is incorrect type (string or list<string>)');
      is.valid <- FALSE;
    }
  } else {
    warning('Validation error: \'sex\' is missing');
    is.valid <- FALSE;
  }

  # Field: valid_event_data_range
  if (!is.null(data$valid_event_data_range) && !validate_type(data$valid_event_data_range, 'string')) {
    warning('Validation error: \'valid_event_data_range\' is incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: agreement_date
  if (!is.null(data$agreement_date) && !validate_type(data$agreement_date, 'string')) {
    warning('Validation error: \'agreement_date\' is incorrect type (string)')
  }

  # Field: type
  if (!is.null(data$type) && validate_type(data$type, 'string')) {
    phenotype.type <- trimws(data$type);
    if (!phenotype.type %in% API_PHENOTYPE_VALIDATION$TYPES) {
      warning('Validation error: \'type\' is not within valid range (Disease or Syndrome, Biomarker, Lifestyle Risk Factor)');
      is.valid <- FALSE;
    }
  } else {
    warning('Validation error: \'type\' is missing or incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: tags
  if (!is.null(data$tags)) {
    if (is.vector(data$tags)) {
      for (phenotype.tag in data$tags) {
        if (validate_type(phenotype.tag, 'number')) {
          if (!exists_tag(phenotype.tag, api_client)) {
            warning('Validation error: \'tags\' id does not exist');
            is.valid <- FALSE;
          }
        } else {
          warning('Validation error: \'tags\' list element is incorrect type (int)');
          is.valid <- FALSE;
        }
      }
    } else if (validate_type(data$tags, 'number')) {
      if (!exists_tag(phenotype.tag, api_client)) {
        warning('Validation error: \'tags\' id does not exist');
        is.valid <- FALSE;
      }
    } else {
      warning('Validation error: \'tags\' is incorrect type (int or list<int>)');
      is.valid <- FALSE;
    }
  }

  # Field: collections
  if (!is.null(data$collections)) {
    if (is.vector(data$collections)) {
      for (phenotype.tag in data$collections) {
        if (validate_type(phenotype.tag, 'number')) {
          if (!exists_collection(phenotype.tag, api_client)) {
            warning('Validation error: \'collections\' id does not exist');
            is.valid <- FALSE;
          }
        } else {
          warning('Validation error: \'collections\' list element is incorrect type (int)');
          is.valid <- FALSE;
        }
      }
    } else if (validate_type(data$collections, 'number')) {
      if (!exists_collection(phenotype.tag, api_client)) {
        warning('Validation error: \'collections\' id does not exist');
        is.valid <- FALSE;
      }
    } else {
      warning('Validation error: \'collections\' is incorrect type (int or list<int>)');
      is.valid <- FALSE;
    }
  }

  # Field: description
  if (!is.null(data$description) && !validate_type(data$description, 'string')) {
    warning('Validation error: \'description\' is incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: implementation
  if (!is.null(data$implementation) && !validate_type(data$implementation, 'string')) {
    warning('Validation error: \'implementation\' is incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: validation
  if (!is.null(data$validation)) {
    is.valid.validations <- lapply(data$validation, function (x) { x <- !validate_type(x, 'string') });
    if (any(as.logical(is.valid.validations))) {
      warning('Validation error: \'validation\' is incorrect type (string)');
      is.valid <- FALSE;
    } else {
      is.valid.validations <- lapply(data$validation, function (x) { x <- !(tolower(x) %in% API_PHENOTYPE_VALIDATION$VALIDATION) });
      if (any(as.logical(is.valid.validations))) {
        warning('Validation error: \'validation\' is incorrect, see valid inputs')
        is.valid <- FALSE;
      }
    }
  }


  # Field: phenoflowid
  if (!is.null(data$phenoflowid) && ((validate_type(data$phenoflowid, 'string') && data$phenoflowid != '') || !validate_type(data$phenoflowid, 'number'))) {
    warning('Validation error: \'phenoflowid\' is incorrect type (int)');
    is.valid <- FALSE;
  }

  # Field: primary_publication
  if (!is.null(data$primary_publication) && !validate_type(data$primary_publication, 'string')) {
    warning('Validation error: \'primary_publication\' is incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: primary_publication_link
  if (!is.null(data$primary_publication_link) && !validate_type(data$primary_publication_link, 'string')) {
    warning('Validation error: \'primary_publication_link\' is incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: primary_publication_doi
  if (!is.null(data$primary_publication_doi) && !validate_type(data$primary_publication_doi, 'string')) {
    warning('Validation error: \'primary_publication_doi\' is incorrect type (string)');
    is.valid <- FALSE;
  }

  # Field: publications
  if (!is.null(data$publications)) {
    is.valid.publications <- lapply(data$publications, function (x) { x <- !validate_type(x, 'string') });
    if (any(as.logical(is.valid.publications))) {
      warning('Validation error: \'publications\' is incorrect type (string or list<string>)');
      is.valid <- FALSE;
    }
  }

  # Field: concepts
  is.valid <- api_validate_phenotype_concepts(data, api_client, is.update, is.valid);

  # Field: data_sources
  is.valid <- api_validate_phenotype_datasources(data, api_client, is.valid);

  return (is.valid);
}
