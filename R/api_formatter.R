#' api_format_phenotype
#'
#' Formats phenotype data to standard accepted by the Concept Library API.
#'
#' @param phenotype.data Unformatted phenotype data
#'
#' @return Formatted phenotype data
#'
api_format_phenotype <- function (phenotype.data) {
  formatted.data <- API_PHENOTYPE_FORMAT;
  formatted.data$phenotype_uuid <- phenotype.data$phenotype_uuid;
  formatted.data$title <- phenotype.data$title;
  formatted.data$name <- phenotype.data$title;
  formatted.data$type <- phenotype.data$type;
  formatted.data$sex <- phenotype.data$sex;
  formatted.data$publish_immediately <- phenotype.data$publish;

  if (!is.null(phenotype.data$phenoflow_id)) {
    formatted.data$phenoflowid <- phenotype.data$phenoflowid;
  } else {
    formatted.data$phenoflowid <- "";
  }

  if (is.list(phenotype.data$author)) {
    formatted.data$author <- do.call(paste, c(as.list(phenotype.data$author), sep=', '));
  } else {
    formatted.data$author <- phenotype.data$author;
  }

  if (!is.null(phenotype.data$description)) {
    formatted.data$description <- phenotype.data$description;
  } else {
    formatted.data$description <- '';
  }

  if (!is.null(phenotype.data$valid_event_data_range)) {
    formatted.data$valid_event_data_range <- phenotype.data$valid_event_data_range;
  } else {
    formatted.data$valid_event_data_range <- '';
  }

  if (!is.null(phenotype.data$primary_publication_doi)) {
    formatted.data$primary_publication_doi <- phenotype.data$primary_publication_doi;
  } else {
    formatted.data$primary_publication_doi <- '';
  }

  if (!is.null(phenotype.data$primary_publication_link)) {
    formatted.data$primary_publication_link <- phenotype.data$primary_publication_link;
  } else {
    formatted.data$primary_publication_link <- '';
  }

  if (formatted.data$publication_doi != '' || formatted.data$publication_link != '') {
    formatted.data$paper_published = TRUE;
  }

  if (!is.null(phenotype.data$publications)) {
    if (!is.list(phenotype.data$publications)) {
      formatted.data$publications <- as.list(phenotype.data$publications);
    } else {
      formatted.data$publications <- phenotype.data$publications;
    }
  } else {
    formatted.data$publications <- list();
  }

  if (!is.null(phenotype.data$validation)) {
    formatted.data$validation <- phenotype.data$validation;
    if (formatted.data$validation != '') {
      formatted.data$validation_performed = TRUE;
    }
  } else {
    formatted.data$validation <- '';
  }

  if (!is.null(phenotype.data$tags)) {
    if (!is.list(phenotype.data$tags)) {
      formatted.data$tags <- as.list(phenotype.data$tags);
    } else {
      formatted.data$tags <- phenotype.data$tags;
    }
  } else {
    formatted.data$tags <- list();
  }

  if (!is.null(phenotype.data$collections)) {
    if (!is.list(phenotype.data$collections)) {
      formatted.data$tags <- append(formatted.data$tags, as.list(phenotype.data$collections));
    } else {
      formatted.data$tags <- list(formatted.data$tags, phenotype.data$collections);
    }
  }

  if (!is.null(phenotype.data$data_sources)) {
    if (!is.list(phenotype.data$data_sources)) {
      formatted.data$data_sources <- as.list(phenotype.data$data_sources);
    } else {
      formatted.data$data_sources <- phenotype.data$data_sources;
    }
  } else {
    formatted.data$data_sources <- list();
  }

  return (formatted.data);
}

#' api_format_concept
#'
#' Formats concept data to standard accepted by the Concept Library API.
#'
#' @param concepts.data List of unformatted concept data
#' @param phenotype.data Formatted parent phenotype data
#'
#' @return Formatted concept data (can return concept ID if concept type is
#'   existing_phenotype)
#'
api_format_concept <- function (concepts.data, phenotype.data) {
  result <- list();

  concept.index <- 1;
  for (concept in concepts.data) {
    params <- do.call(list, unlist(concept[[1]], recursive=FALSE));
    if (params$type == 'existing_concept') {
      new.concept <- list();
      new.concept$name <- names(concept);
      new.concept$id <- params$concept_id;
    } else {
      new.concept <- API_CONCEPT_FORMAT;
      new.concept$name <- names(concept);
      new.concept$author <- phenotype.data$author;
      new.concept$paper_published <- phenotype.data$paper_published;
      new.concept$publication_link <- phenotype.data$publication_link;
      new.concept$publication_doi <- phenotype.data$publication_link;
      new.concept$validation_performed <- phenotype.data$validation_performed;
      new.concept$validation_description <- phenotype.data$validation;
      new.concept$tags <- phenotype.data$tags;
      new.concept$publish_immediately <- phenotype.data$publish;

      if (params$type == 'csv') {
        new.component <- API_COMPONENT_FORMAT;
        new.component$name <- qq('CODES - @{new.concept$name}');
        new.component$codes <- list();

        codes.index <- 1;
        for (code in read_file(params$filepath)$CODE) {
          new.component$codes[[codes.index]] <- API_CODE_FORMAT;
          new.component$codes[[codes.index]]$code <- code;

          codes.index <- codes.index + 1;
        }

        # Add to concept
        new.concept$coding_system <- API_CODING_SYSTEM_LOOKUP[[params$coding_system]];
        new.concept$components <- list(new.component);
      } else if (params$type == 'inline') {
        new.component <- API_COMPONENT_FORMAT;
        new.component$name <- qq('CODES - @{new.concept$name}');
        new.component$codes <- list();

        codelist <- list();
        if (is.list(params$codes) || (is.character(params$codes) && length(params$codes) != 1)) {
          codelist <- params$codes;
        } else if (validate_type(params$codes, 'string')) {
          codelist <- as.list(strsplit(trimws(params$codes), ',')[[1]])
        }

        codes.index <- 1;
        for (code in codelist) {
          new.component$codes[[codes.index]] <- API_CODE_FORMAT;
          new.component$codes[[codes.index]]$code <- trimws(code);

          codes.index <- codes.index + 1;
        }

        # Add to concept
        new.concept$coding_system <- API_CODING_SYSTEM_LOOKUP[[params$coding_system]];
        new.concept$components <- list(new.component);
      }
    }

    result[[concept.index]] <- new.concept;
    concept.index <- concept.index + 1;
  }

  return (result);
}
