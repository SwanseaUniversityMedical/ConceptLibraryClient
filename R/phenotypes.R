#' Phenotypes
#'
#' @description
#' Phenotypes object, inheriting from ConceptLibraryClient::Endpoint - allows
#'  querying of phenotypes/ endpoints
#'
Phenotypes <- R6::R6Class(
  'Phenotypes',
  inherit = Endpoint,
  public = list(
    #' @description
    #' Queries phenotypes/, with optional query parameters
    #'
    #' @param ... (list) List of optional parameters
    #'
    #' @returns Response object
    #'
    get = function (...) {
      query_params = list(...)

      url = super$get_full_path('PHENOTYPES', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #' @description
    #' Queries phenotypes/{id}/get-versions/
    #'
    #' @param phenotype_id (string) Id of entity to query
    #'
    #' @returns Response object
    #'
    get_versions = function (phenotype_id) {
      url = super$get_full_path('PHENOTYPES', 'VERSION_HISTORY', id=phenotype_id)
      return (super$make_request('get', url))
    },

    #' @description
    #' Queries phenotypes/{id}/detail/ or phenotypes/{id}/version/{id}/detail/
    #'
    #' @param phenotype_id (string) Id of entity to query
    #' @param version_id (integer) Version id of entity to query
    #'
    #' @returns Response object
    #'
    get_detail = function (phenotype_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'PHENOTYPES', url, id=phenotype_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    },

    #' @description
    #' Queries phenotypes/{id}/export/codes/ or
    #'  phenotypes/{id}/version/{id}/export/codes/
    #'
    #' @param phenotype_id (string) Id of entity to query
    #' @param version_id (integer) Version id of entity to query
    #'
    #' @returns Response object
    #'
    get_codelist = function (phenotype_id, version_id=NA) {
      url = if (is.na(version_id)) 'CODELIST' else 'CODELIST_BY_VERSION'
      url = super$get_full_path(
        'PHENOTYPES', url, id=phenotype_id, version_id=version_id
      )

      return (super$make_request('get', url))
    },

    #' @description
    #' Formats phenotype detail and saves it to file
    #'
    #' @param path (string) Path to save the file
    #' @param phenotype_id (string) Id of entity to query
    #' @param version_id (integer) Version id of entity to query
    #'
    save_definition_file = function (path, phenotype_id, version_id=NA) {
      phenotype_data = self$get_detail(phenotype_id, version_id=version_id)
      phenotype_data = private$prepare_phenotype_data(phenotype_data)

      yaml::write_yaml(phenotype_data, path)
    },

    #' @description
    #' Creates a new phenotype based on the .yaml file supplied
    #'
    #' @param path (string) Path to definition file
    #'
    #' @returns Response object
    #'
    create = function (path) {
      data = private$read_phenotype_definition(path)
      data = private$format_phenotype(data)

      url = super$get_full_path('PHENOTYPES', 'CREATE')
      response = super$make_request(
        'post', url, body=data, as_df=FALSE
      )
      response = response$entity

      self$save_definition_file(path, response$id, version_id=response$version_id)

      return (response)
    },

    #' @description
    #' Updates an existing phenotype with details from the .yaml file supplied
    #'
    #' @param path (string) Path to definition file
    #'
    #' @returns Response object
    #'
    update = function (path) {
      data = private$read_phenotype_definition(path)
      data = private$format_phenotype(data, update=TRUE)

      url = super$get_full_path('PHENOTYPES', 'UPDATE')
      response = super$make_request(
        'put', url, body=data, as_df=FALSE
      )
      response = response$entity

      self$save_definition_file(path, response$id, version_id=response$version_id)

      return (response)
    }
  ),

  private = list(
    #' @field ALLOWED_FILE_EXTENSIONS (list) List of file extensions allowed
    ALLOWED_FILE_EXTENSIONS = list('yaml', 'yml'),

    #' @field PHENOTYPE_IGNORE_FILEDS (list) List of fields to ignore
    PHENOTYPE_IGNORE_FIELDS = list(
      WRITE = list(
        'owner', 'phenotype_id', 'phenotype_version_id',
        'created', 'updated', 'template'
      ),
      READ = list(
        'versions', 'status', 'is_deleted', 'owner_access', 'coding_system',
        'publish_status', 'created', 'updated'
      )
    ),

    #' @description
    #' Reads in a file, validating existence and extension against allowed
    #'  extensions, stops if validation failed
    #'
    #' @param path (string) Path to file to be read in
    #'
    #' @returns Data read in from the file
    #'
    read_phenotype_definition = function (path) {
      data = read_file(path, yaml::read_yaml, extensions=private$ALLOWED_FILE_EXTENSIONS)
      if (is.null(data) || length(data) <= 1) {
        stop(sprintf(
          'File is invalid, please check the file location and file extension, supports: %s',
          private$ALLOWED_FILE_EXTENSIONS
        ))
      }

      return (data)
    },

    #' @description
    #' Formats phenotype into format acceptable for the create/update endpoints
    #'
    #' @param data (list) Phenotype data
    #' @param update (bool) Whether the request is an update
    #'
    #' @returns Formatted phenotype data
    #'
    format_phenotype = function (data, update=FALSE) {
      result = list(data = data)

      if (update) {
        result$entity = list(
          id = result$data$phenotype_id
        )
      }

      if ('concept_information' %in% names(result$data)) {
        result$data$concept_information = private$format_concepts(result$data$concept_information)
      }

      if ('publications' %in% names(result$data)) {
        result$data$publications = try_parse_doi(result$data$publications)
      }

      result$template = list(
        id = result$data$template$id,
        version = result$data$template$version_id
      )

      result$data[sapply(names(result$data), function(x) x %in% private$PHENOTYPE_IGNORE_FIELDS$WRITE)] = NULL

      return (result)
    },

    #' @description
    #' Formats concept into format acceptable for the create/update endpoints
    #'
    #' @param data (list) Concept data
    #'
    #' @returns Formatted concept data
    #'
    format_concepts = function (data) {
      concept_information = list()
      for (concept in data) {
        if (concept$type == 'existing_concept') {
          new_concept = list(
            name = concept$name,
            concept_id = concept$concept_id,
            concept_version_id = concept$concept_version_id,
            internal_type = concept$type
          )

          concept_information = append(
            concept_information,
            list(new_concept)
          )

          next
        }

        if (concept$type == 'csv') {
          concept_information = append(
            concept_information,
            list(private$format_concept_from_csv(concept))
          )

          next
        }
      }

      return (concept_information)
    },

    #' @description
    #' Formats a concept when type='csv'
    #'
    #' @param data (list) Concept data
    #'
    #' @returns Formatted concept data
    #'
    format_concept_from_csv = function (data) {
      new_concept = list(
        details = list(
          name = data$name,
          coding_system = data$coding_system,
          internal_type = data$type,
          code_attribute_header = list()
        ),
        components = list()
      )

      if (!is.null(data$concept_id) && !is.null(data$concept_version_id)) {
        new_concept$concept_id = data$concept_id
        new_concept$concept_version_id = data$concept_version_id
        new_concept$is_dirty = TRUE
      } else {
        new_concept$is_new = TRUE
      }

      built_components = private$build_concept_component(data, new_concept)
      new_concept$components = list(built_components$component)

      if (!is.null(built_components$code_attribute_header)) {
        new_concept$details$code_attribute_header = built_components$code_attribute_header
      }

      return (new_concept)
    },

    #' @description
    #' Builds a concept component from linked csv file
    #'
    #' @param data (list) Concept data
    #' @param new_concept (list) New, formatted concept data
    #'
    #' @returns Concept component
    #'
    build_concept_component = function (data, new_concept) {
      codelist_data = read_file(
        data$filepath,
        function (x) read.csv(x, check.names=FALSE),
        'csv'
      )
      if (is.null(codelist_data) || length(codelist_data) <= 1) {
        stop(sprintf(
          'Concept %s has missing or invalid codelist file',
          new_concept$details$name
        ))
      }

      code_column = data$code_column
      description_column = data$description_column
      if (is.null(code_column) || !(code_column %in% names(codelist_data))) {
        stop(sprintf(
          'Concept %s has missing or invalid code_column',
          new_concept$details$name
        ))
      }

      attribute_headers = names(codelist_data)
      attribute_headers = attribute_headers[!attribute_headers %in% c(code_column, description_column)]

      new_component = list(
        is_new = TRUE,
        name = sprintf('CODES - %s', new_concept$details$name),
        logical_type = 'INCLUDE',
        source_type = 'FILE_IMPORT',
        codes = list()
      )
      for (index in 1:nrow(codelist_data)) {
        code = codelist_data[index, code_column]

        description = ''
        if (!is.null(description_column)) {
          description = codelist_data[index, description_column]
        }

        new_component$codes = append(
          new_component$codes,
          list(list(
            code = code,
            description = description,
            attributes = as.character(codelist_data[index, !names(codelist_data) %in% c(code_column, description_column)])
          ))
        )
      }

      return (list(
        components = new_component,
        code_attribute_header = attribute_headers
      ))
    },

    #' @description
    #' Prepares phenotype data in format that's acceptable for the cohort
    #'  definition file
    #'
    #' @param data (list) Phenotype data
    #'
    #' @returns Formatted phenotype data
    #'
    prepare_phenotype_data = function (data) {
      result = list()
      for (field in names(data)) {
        if (field %in% private$PHENOTYPE_IGNORE_FIELDS$READ) {
          next
        }

        field_data = data[[field]]
        if (is_empty(field_data)) {
          next
        }

        if (!is.list(field_data)) {
          result[[field]] = field_data
          next
        }

        if (field == 'concept_information') {
          field_data = field_data[[1]]

          result[[field]] = list()
          for (i in 1:nrow(field_data)) {
            concept = field_data[i,]

            formatted_concept = list(
              name = concept$concept_name,
              type = 'existing_concept',
              concept_id = concept$concept_id,
              concept_version_id = concept$concept_version_id
            )

            result[[field]] = append(result[[field]], list(formatted_concept))
          }

          next
        }

        if (field == 'group') {
          result[[field]] = field_data[['id']]
          next
        }

        if (is.null(names(field_data))) {
          field_data = field_data[[1]]
        }

        field_params = names(field_data)
        if ('value' %in% field_params) {
          value = as.list(field_data$value)
          if (length(value) == 1) {
            value = unlist(value)
          }
          result[[field]] = value
        } else if ('id' %in% field_params && 'version_id' %in% field_params) {
          result[[field]] = list(
            id = field_data$id,
            version_id = field_data$version
          )
        } else if ('doi' %in% field_params && 'details' %in% field_params) {
          result[[field]] = list()

          for (i in 1:nrow(field_data)) {
            result[[field]][i] = field_data[i, 'details']
          }
        } else if (is.data.frame(field_data)) {
          result[[field]] = list()

          for (i in 1:nrow(field_data)) {
            item = field_data[i,]

            result[[field]][[i]] = list()
            for (key in names(item)) {
              result[[field]][[i]][[key]] = item[[key]]
            }
          }
        }
      }

      return (result)
    }
  )
)
