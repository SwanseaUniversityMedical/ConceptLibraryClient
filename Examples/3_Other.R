library(ConceptLibraryClient)

# Connect to the ConceptLibrary

## Authenticated connection, login required
client = ConceptLibraryClient::Connection$new(
  username='my-username',
  password='my-password',
  url=ConceptLibraryClient::DOMAINS$SAIL
)

## Public connection, no account required
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$SAIL
)


# Querying collections

## Get all collections
collection_list = client$collections$get()

## Get the collections detail
collection_detail = client$collections$get_detail(18)


# Querying tags

## Get all tags
tag_list = client$tags$get()

## Get the tags detail
tag_detail = client$tags$get_detail(1)


# Querying datasources

## Get all datasources
datasource_list = client$datasources$get()

## Get the datasources detail
datasource_detail = client$datasources$get_detail(1)


# Querying ontology

## Get paginated list of ontological term(s) (nodes; paginated by default)
search_results = client$ontology$get()

## [NOTE: not recommended!] Get unpaginated list of ontological term(s)
search_results = client$ontology$get(no_pagination=TRUE)

## Iterating through ontology result pages

### Search ontological terms (add parameters, and optionally declare `page=1`)
search_results = client$ontology$get()

### Get the page attribute list, e.g. ...
search_info = attributes(search_results)

### Use the current page number, or append your own `page=[number]` parameter
search_results = client$ontology$get(page=search_info$page + 1)

## Search ontological term(s) (paginated by default, apply `no_pagination=T` if required)
## [!] See the following for more information on parameters:
##     https://conceptlibrary.saildatabank.com/api/v1/#operations-tag-ontology
##
search_results = client$ontology$get(
  # Searches across names, descriptions & synonyms
  search='dementia',

  # Search by snomed codes (or ICD9/10, OPSC4, ReadCode2/3 etc)
  #  - Note: this will fuzzy match across all code mappings;
  #          apply the `exact_codes=T` parameter if you want exact matches
  codes='281004'
)

## Get a specific ontology node by ID
ontology_node = client$ontology$get_detail(1)

## Get all ontology groups available
ontology_groups = client$ontology$get_groups()

## Get the ontology group's detail
clin_speciality_list = client$ontology$get_group_detail(1)

