Phenotypes <- R6::R6Class(
  'Phenotypes',
  inherit = Endpoint,
  public = list(
    #'
    get = function (...) {
      query_params = super$clean_query_params(...)

      url = super$get_full_path('PHENOTYPES', 'INDEX')
      return (super$make_request('get', url, query=query_params))
    },

    #'
    get_versions = function (phenotype_id) {
      url = super$get_full_path('PHENOTYPES', 'VERSION_HISTORY', id=phenotype_id)
      return (super$make_request('get', url))
    },

    #'
    get_detail = function (phenotype_id, version_id=NA) {
      url = if (is.na(version_id)) 'DETAIL' else 'DETAIL_BY_VERSION'
      url = super$get_full_path(
        'PHENOTYPES', url, id=phenotype_id, version_id=version_id
      )

      return (super$make_request('get', url, as_df=FALSE))
    },

    #'
    get_codelist = function (phenotype_id, version_id=NA) {
      url = if (is.na(version_id)) 'CODELIST' else 'CODELIST_BY_VERSION'
      url = super$get_full_path(
        'PHENOTYPES', url, id=phenotype_id, version_id=version_id
      )

      return (super$make_request('get', url))
    },

    #'
    save_to_file = function (path, phenotype_id, version_id=NA) {
      phenotype_data = self$get_detail(phenotype_id, version_id=version_id)
      phenotype_data = private$prepare_phenotype_data(phenotype_data)

      yaml::write_yaml(phenotype_data, path)
    },

    #'
    create = function (path) {
      data = private$read_phenotype_definition(path)
      data = private$format_phenotype(data)

      url = super$get_full_path('PHENOTYPES', 'CREATE')
      response = super$make_request(
        'post', url, body=data, as_df=FALSE
      )
      response = response$entity

      self$save_to_file(path, response$id, version_id=response$version_id)

      return (response)
    },

    #'
    update = function (path) {
      data = private$read_phenotype_definition(path)
      data = private$format_phenotype(data, update=TRUE)

      url = super$get_full_path('PHENOTYPES', 'UPDATE')
      response = super$make_request(
        'put', url, body=data, as_df=FALSE
      )
      response = response$entity

      self$save_to_file(path, response$id, version_id=response$version_id)

      return (response)
    }
  ),

  private = list(
    #'
    ALLOWED_FILE_EXTENSIONS = list('yaml', 'yml'),

    #'
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

      result$template = list(
        id = result$data$template$id,
        version = result$data$template$version
      )

      result$data[sapply(names(result$data), function(x) x %in% private$PHENOTYPE_IGNORE_FIELDS$WRITE)] = NULL

      return (result)
    },

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

    #'
    format_concept_from_csv = function (data) {
      new_concept = list(
        details = list(
          name = data$name,
          coding_system = data$coding_system,
          internal_type = data$type
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

      new_component = private$build_concept_component(data, new_concept)
      new_concept$components = list(new_component)

      return (new_concept)
    },

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
            description = description
          ))
        )
      }

      return (new_component)
    },

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
        } else if (is.data.frame(field_data)) {
          result[[field]] = list()

          for (i in 1:nrow(field_data)) {
            item = field_data[[i,]]

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
