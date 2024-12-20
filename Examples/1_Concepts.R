library(ConceptLibraryClient)

# Connect to the ConceptLibrary

## Authenticated connection, login required
client = ConceptLibraryClient::Connection$new(
  url=ConceptLibraryClient::DOMAINS$SAIL
)

## Public connection, no account required
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$SAIL
)


# Querying concepts

## Get paginated list of concepts
search_results = client$concepts$get()

## [NOTE: not recommended!] Get unpaginated list of concepts
search_results = client$concepts$get(no_pagination=TRUE)

## Iterating through concept result pages

### List concepts (add parameters, and optionally declare `page=1`)
search_results = client$concepts$get()

### Get page-related information from a search result
page = attr(search_results, 'page')
page_size = attr(search_results, 'page_size')
total_pages = attr(search_results, 'total_pages')

### Or get the page attribute list, e.g. ...
search_info = attributes(search_results)
page = search_info$page

### Use the current page number, or append your own `page=[number]` parameter
search_results = client$concepts$get(page=page+1)

## Search concepts (paginated by default, apply `no_pagination=T` if required)
search_results = client$concepts$get(
  search='asthma',
  collections=19
)

## Get a concepts's version history
concept_versions = client$concepts$get_versions('C714')

## Get details
concept_detail = client$concepts$get_detail('C714')

## Get the details of a specific version of the concept
concept_detail = client$concepts$get_detail('C714', version_id=2567)

## Get codelist
concept_codelist = client$concepts$get_codelist('C714')

## Get the codelist of a specific version of the concept
concept_codelist = client$concepts$get_codelist('C714', version_id=2567)
