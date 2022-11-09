#' api_create_phenotype
#'
#' Creates phenotype using formatted data from uploaded yaml file.
#'
#' @param data Formatted phenotype data.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function
#'
#' @returns TRUE if successful, FALSE otherwise
#'
api_create_phenotype <- function (data, api_client) {
  path = get_full_path(qq('api_phenotype_create/'), api_client)

  response = api_client$post(
    path = path,
    body = data,
    encode = 'json'
  );

  check_HTTP_response(response);

  content <- jsonlite::fromJSON(response$parse('utf-8'));
  print(paste(content$message, ':', content$id));

  return (content$id);
}

#' api_create_concept
#'
#' Creates concept using formatted data from uploaded yaml file.
#'
#' @param data Formatted concept data.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function
#'
#' @returns TRUE if successful, FALSE otherwise
#'
api_create_concept <- function (data, api_client) {
  path = get_full_path(qq('api_concept_create/'), api_client)

  response = api_client$post(
    path = path,
    body = data,
    encode = 'json'
  );

  check_HTTP_response(response);

  content <- jsonlite::fromJSON(response$parse('utf-8'));
  print(paste(content$message, ':', content$id));

  return (content$id);
}

#' api_update_phenotype
#'
#' Updates phenotype using formatted data from uploaded yaml file.
#'
#' @param data Formatted phenotype data.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function
#'
#' @returns TRUE if successful, FALSE otherwise
#'
api_update_phenotype <- function (data, api_client) {
  path = get_full_path(qq('api_phenotype_update/'), api_client)

  response = api_client$put(
    path = path,
    body = data,
    encode = 'json'
  );

  check_HTTP_response(response);

  content <- jsonlite::fromJSON(response$parse('utf-8'));
  print(paste(content$message, ':', content$id));

  return (content$id);
}

#' api_update_concept
#'
#' Updates concept using formatted data from uploaded yaml file.
#'
#' @param data Formatted concept data.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function
#'
#' @returns TRUE if successful, FALSE otherwise
#'
api_update_concept <- function (data, api_client) {
  path = get_full_path(qq('api_concept_update/'), api_client)

  response = api_client$put(
    path = path,
    body = data,
    encode = 'json'
  );

  check_HTTP_response(response);

  content <- jsonlite::fromJSON(response$parse('utf-8'));
  print(paste(content$message, ':', content$id));

  return (content$id);
}

#' update_phenotype
#'
#' Updates existing phenotypes and concepts based on provided yaml file.
#'
#' @param file.path File path pointing to yaml file containing phenotype and concept data
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function
#'
#' @return List of updated, formatted phenotype and concept data
#'
update_phenotype <- function(file.path, api_client) {
  data <- read_file(file.path);
  if (!is.null(data) && length(data) > 1) {
    is.valid <- api_validate_phenotype(data, api_client, TRUE);
    if (is.valid) {
      new.phenotype <- api_format_phenotype(data);
      new.concepts <- api_format_concept(data$concepts, new.phenotype);

      concept.ids <- list();
      concept.ids.raw <- list();
      for (concept in new.concepts) {
        if (concept$internal_type != 'existing_concept') {
          api_update_concept(concept, api_client);
        }

        # Formatted for output
        formatted.concept.id <- list();
        formatted.concept.id[[concept$name]] <- concept$id;
        concept.ids <- append(concept.ids, formatted.concept.id);

        # Raw for input to phenotype upload
        concept.ids.raw <- append(
          concept.ids.raw,
          substr(concept$id, 2, nchar(concept$id))
        );
      }
      new.phenotype$concept_informations <- concept.ids.raw;

      # Upload phenotypes
      phenotype.id <- api_update_phenotype(new.phenotype, api_client);

      # Write to output file
      write_yaml_file(
        data,
        phenotype.id,
        concept.ids,
        file.path, #'./templates/outputs/result.yaml',
        api_client
      );

      return (list(
        phenotype.id=phenotype.id,
        phenotype=new.phenotype,
        concept.ids=concept.ids,
        concepts=new.concepts
      ));
    } else {
      stop('Invalid phenotype template, please resolve the highlighted warnings');
    }
  } else {
    stop('Phenotype file is invalid, please check the file location and filetype (supports .yaml or .yml files only)')
  }
}

#' upload_phenotype
#'
#' Reads yaml file containing phenotype and concept data, validates, formats and uploads to the Concept Library.
#'
#' @param file.path File path pointing to yaml file containing phenotype and concept data
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function
#'
#' @return List of formatted phenotype and concept data, including uploaded ids
#'
#' @export
#'
upload_phenotype <- function(file.path, api_client) {
  data <- read_file(file.path);
  if (length(data) > 1) {
    is.valid <- api_validate_phenotype(data, api_client, FALSE);
    if (is.valid) {
      new.phenotype <- api_format_phenotype(data);
      new.concepts <- api_format_concept(data$concepts, new.phenotype);

      # Upload concepts
      concept.ids <- list();
      concept.ids.raw <- list();
      for (concept in new.concepts) {
        if (concept$internal_type == 'existing_concept') {
          # Formatted for output
          formatted.concept.id <- list();
          formatted.concept.id[[concept$name]] <- concept$id;
          concept.ids <- append(concept.ids, formatted.concept.id);

          # Raw for input to phenotype upload
          concept.ids.raw <- append(
            concept.ids.raw,
            substr(concept$id, 2, nchar(concept$id))
          );
        } else {
          uploaded.concept.id <- api_create_concept(concept, api_client);

          # Formatted for output
          formatted.concept.id <- list();
          formatted.concept.id[[concept$name]] <- paste('C', uploaded.concept.id, sep='');
          concept.ids <- append(concept.ids, formatted.concept.id);

          # Raw for input to phenotype upload
          concept.ids.raw <- append(
            concept.ids.raw,
            uploaded.concept.id
          );
        }
      }
      new.phenotype$concept_informations <- concept.ids.raw;

      # Upload phenotypes
      phenotype.id <- api_create_phenotype(new.phenotype, api_client);

      # Write to output file
      write_yaml_file(
        data,
        phenotype.id,
        concept.ids,
        file.path, #'./templates/outputs/result.yaml',
        api_client
      );

      return (list(
        phenotype.id=phenotype.id,
        phenotype=new.phenotype,
        concept.ids=concept.ids,
        concepts=new.concepts
      ));
    } else {
      stop('Invalid phenotype template, please resolve the highlighted warnings');
    }
  }
}
