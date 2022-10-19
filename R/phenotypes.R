#' get_phenotypes
#'
#' Lists all available phenotypes for the user and the data sources associate with each.
#'
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#' @param search Search by part of phenotype name (do not put wild characters here)
#' @param tag_ids Specify vector of tags ids (get tags from get_tags())
#' @param collection_ids Specify vector of collection ids (get collections from get_collections())
#' @param show_only_my_phenotypes Only show phenotypes owned by me. Default is FALSE. Can't be used with public API.
#' @param show_deleted_phenotypes Also show deleted phenotypes. Default is FALSE. Can't be used with public API.
#' @param show_only_validated_phenotypes Show only validated phenotypes. Default is FALSE.
#' @param brand Show only phenotypes with a specified brand.
#' @param author Search by part of the author name.
#' @param owner_username Search by full username of the owner. Can't be used with public API.
#' @param do_not_show_versions Do not show phenotypes versions. Default is FALSE (versions are shown).
#' @param must_have_published_versions Show only phenotypes which have a published version. Default is FALSE. Can't be
#' used with public API.
#'
#' @return A dataframe containing the phenotypes matching the query.
#' @export
#'
#' @examples
#' get_phenotypes()
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotypes(api_client = api_client)
#' get_phenotypes(
#'   api_client = api_client,
#'   search = 'Alcohol',
#'   tag_ids = c(1,4),
#'   collection_ids = C(19, 20),
#'   show_only_my_phenotypes = TRUE,
#'   show_deleted_phenotypes = TRUE,
#'   show_only_validated_phenotypes = TRUE,
#'   brand = 'HDRUK',
#'   author = 'Kuan',
#'   owner_username = 'a.john',
#'   do_not_show_versions = TRUE,
#'   must_have_published_versions = TRUE)
#'
get_phenotypes <- function(
  api_client = connect_to_API(),
  search = NA,
  tag_ids = NA,
  collection_ids = NA,
  show_only_my_phenotypes = FALSE,
  show_deleted_phenotypes = FALSE,
  show_only_validated_phenotypes = FALSE,
  brand = NA,
  author = NA,
  owner_username = NA,
  do_not_show_versions = FALSE,
  must_have_published_versions = FALSE
) {
  # Create list of named query parameters
  query_params = list()
  if (is_connection_authenticated(api_client)) {
    query_params = list(
      search = search,
      tag_ids = tag_ids,
      collection_ids = collection_ids,
      show_only_my_phenotypes = show_only_my_phenotypes,
      show_deleted_phenotypes = show_deleted_phenotypes,
      show_only_validated_phenotypes = show_only_validated_phenotypes,
      brand = brand,
      author = author,
      owner_username = owner_username,
      do_not_show_versions = do_not_show_versions,
      must_have_published_versions =  must_have_published_versions
    )
  } else {
    # Throw error if invalid query parameters were given for public API
    if (isTRUE(show_only_my_phenotypes) || isTRUE(show_deleted_phenotypes) || !is.na(owner_username)
        || isTRUE(must_have_published_versions)) {
      stop("One or more of the parameters specified in get_phenotypes() cannot be used with the public API. Use
           connect_to_API(public=FALSE) to create an authenticated connection, or check the documentation with
           ?ConceptLibraryClient::get_phenotypes to see which parameters can be used with the public API.")
    }
    query_params = list(
      search = search,
      tag_ids = tag_ids,
      collection_ids = collection_ids,
      show_only_validated_phenotypes = show_only_validated_phenotypes,
      brand = brand,
      author = author,
      do_not_show_versions = do_not_show_versions
    )
  }

  # Clean query parameters to remove NA and FALSE values and change TRUE to 1
  cleaned_params = clean_query_list(query_params)

  # API call with path and query parameters
  path = get_full_path('phenotypes/', api_client)
  response = api_client$get(path = path, query = cleaned_params)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotypes = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotypes)
}

#' get_phenotype_by_id
#'
#' Lists a phenotype by id and the data sources associated with it.
#'
#' @param id The phenotype's id.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the phenotype.
#' @export
#'
#' @examples
#' get_phenotype_by_id('PH1')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_by_id('PH1', api_client = api_client)
#'
get_phenotype_by_id <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotype = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotype)
}

#' get_phenotype_detail
#'
#' Lists the phenotype detail of the latest version (or latest published version if using public API).
#'
#' @param id The phenotype's id.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the phenotype detail.
#' @export
#'
#' @examples
#' get_phenotype_detail('PH1')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_detail('PH1', api_client = api_client)
#'
get_phenotype_detail <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/detail/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotype = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotype)
}

#' get_phenotype_detail_by_version
#'
#' Lists the phenotype detail of the specified version.
#'
#' @param id The phenotype's id.
#' @param version_id The phenotype version's id.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the phenotype detail.
#' @export
#'
#' @examples
#' get_phenotype_detail_by_version('PH1', '2')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_detail_by_version('PH1', '2', api_client = api_client)
#'
get_phenotype_detail_by_version <- function(id, version_id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/version/@{version_id}/detail/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  phenotype = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(phenotype)
}

#' get_phenotype_code_list
#'
#' Exports the code list of a specific version of a phenotype.
#'
#' @param id The phenotype's id.
#' @param version_id The phenotype version's id, defaults to NA. Leave as default if using public api.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the code list.
#' @export
#'
#' @examples
#' get_phenotype_code_list('PH1', '2')
#'
#' api_client = connect_to_API(public = FALSE)
#' get_phenotype_code_list('PH1', '2', api_client = api_client)
#'
get_phenotype_code_list <- function(id, version_id=NA, api_client = connect_to_API()) {
  if (is.na(version_id) || length(api_client$auth) == 0) {
    path = get_full_path(qq('phenotypes/@{id}/export/codes/'), api_client)
  } else {
    path = get_full_path(qq('phenotypes/@{id}/version/@{version_id}/export/codes/'), api_client)
  }

  # API call
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  code_list = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  return(code_list)
}

#' get_phenotype_versions
#'
#' Lists all the versions of the phenotype
#'
#' @param id The phenotype's id.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the phenotype's versions.
#' @export
#'
#' @examples
#' get_phenotype_versions('PH1')
#'
#' api_client = connect_to_API()
#' get_phenotype_versions('PH1', api_client = api_client)
#'
get_phenotype_versions <- function(id, api_client = connect_to_API()) {
  # API call
  path = get_full_path(qq('phenotypes/@{id}/get-versions/'), api_client)
  response = api_client$get(path = path)
  check_HTTP_response(response)

  # Parse JSON result to dataframe
  versions = data.frame(jsonlite::fromJSON(response$parse('utf-8')))

  # In this case the data is contained as a list within the dataframe and needs to be accessed before returning.
  return(versions$versions[[1]])
}

#' exists_phenotype
#'
#' @param id
#' @param api_client
#'
#' @return
#'
exists_phenotype <- function (id, api_client) {
  path = get_full_path(qq('phenotypes/@{id}/detail/'), api_client)
  response = api_client$get(path = path);
  return (response$status_code == 200);
}

#' save_phenotype_definition
#'
#' Saves the Phenotype YAML definition file locally
#'
#' @params dir The directory to save the file to, include the name of the file, e.g. C:/path/definition.yaml
#' @param id The phenotype's id.
#' @param version_id The phenotype version's id, defaults to NA. Leave as default if using public api.
#' @param api_client The HttpClient returned by the \code{\link{connect_to_API}} function. Optional, a public API
#' connection is created if left blank.
#'
#' @return A dataframe containing the phenotype's data
#' @export
#'
#' @examples
#' save_phenotype_definition('C:/path/to/folder', 'definition-file.yaml', 'PH1')
#'
#' api_client = connect_to_API()
#' save_phenotype_definition('C:/path/to/folder', 'definition-file.yaml', 'PH1', '2', api_client)
#'
save_phenotype_definition <- function(dir, id, version_id=NA, api_client = connect_to_API()) {
  if (!validate_type(dir, 'string') || !(grepl('.yaml', dir))) {
    stop('Invalid file name format, must be of type \'.yaml\'')
  }

  phenotype.data <- NA
  if (is.na(version_id) || length(api_client$auth) == 0) {
    phenotype.data <- get_phenotype_detail(id, api_client)
  } else {
    phenotype.data <- get_phenotype_detail_by_version(id, version_id, api_client)
  }

  # Create result object and populate internal fields
  result.data <- list()
  result.data$template_version <- as.character(API_YAML_TEMPLATE_VERSION)
  result.data$phenotype_id <- phenotype.data$phenotype_id
  result.data$phenotype_version_id <- as.character(ifelse(
    !is.na(version_id),
    version_id,
    max(phenotype.data$versions[[1]]$version_id)
  ))

  # Required fields
  result.data$title <- phenotype.data$name
  result.data$type <- phenotype.data$type
  result.data$author <- phenotype.data$author
  result.data$sex <- phenotype.data$sex

  # Concept field
  if (should_write_field(phenotype.data$concepts)) {
    concepts.data <- phenotype.data$concepts[[1]]
    if (nrow(concepts.data) > 0) {
      result.concepts <- list()
      for (row.index in 1:nrow(concepts.data)) {
        cur.data <- list()
        cur.data[[concepts.data[row.index, 'name']]] = list(
          list(type = 'existing_concept'),
          list(concept_id = concepts.data[row.index, 'concept_id'])
        )
        result.concepts <- append(result.concepts, list(cur.data))
      }

      result.data$concepts <- result.concepts
    }
  }

  # Optional fields
  if (should_write_field(phenotype.data$phenotype_uuid)) {
    result.data$phenotype_uuid <- phenotype.data$phenotype_uuid
  }

  if (should_write_field(phenotype.data$valid_event_data_range)) {
    result.data$valid_event_data_range <- phenotype.data$valid_event_data_range
  }

  if (should_write_field(phenotype.data$definition)) {
    result.data$description <- phenotype.data$definition
  }

  if (should_write_field(phenotype.data$implementation)) {
    result.data$implementation <- phenotype.data$implementation
  }

  if (should_write_field(phenotype.data$publications) && length(phenotype.data$publications[[1]]) > 0) {
    result.data$publications <- phenotype.data$publications[[1]]
  }

  if (should_write_field(phenotype.data$publication_link)) {
    result.data$primary_publication_link <- phenotype.data$publication_link
  }

  if (should_write_field(phenotype.data$publication_doi)) {
    result.data$primary_publication_doi <- phenotype.data$publication_doi
  }

  if (should_write_field(phenotype.data$tags) && nrow(phenotype.data$tags[[1]]) > 0) {
    result.data$tags <- as.list(phenotype.data$tags[[1]]$id)
  }

  if (should_write_field(phenotype.data$collections) && nrow(phenotype.data$collections[[1]]) > 0) {
    result.data$collections <- as.list(phenotype.data$collections[[1]]$id)
  }

  if (should_write_field(phenotype.data$data_sources) && nrow(phenotype.data$data_sources[[1]]) > 0) {
    result.data$data_sources <- as.list(phenotype.data$data_sources[[1]]$id)
  }

  # Write to file
  yaml::write_yaml(result.data, dir)

  return (result.data)
}
