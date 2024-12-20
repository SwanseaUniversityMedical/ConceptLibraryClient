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

# Querying phenotypes

## Get paginated list of phenotypes
search_results = client$phenotypes$get()

## [NOTE: not recommended!] Get unpaginated list of phenotypes
search_results = client$phenotypes$get(no_pagination=TRUE)

## Iterating through phenotype result pages

### Search phenotypes (add parameters, and optionally declare `page=1`)
search_results = client$phenotypes$get()

### Get page-related information from a search result
page = attr(search_results, 'page')
page_size = attr(search_results, 'page_size')
total_pages = attr(search_results, 'total_pages')

### Or get the page attribute list, e.g. ...
search_info = attributes(search_results)
page = search_info$page

### Use the current page number, or append your own `page=[number]` parameter
search_results = client$phenotypes$get(page=page+1)

## Search Phenotypes (paginated by default, apply `no_pagination=T` if required)
search_results = client$phenotypes$get(
  search='asthma',
  collections=19
)

## Search Phenotypes by subqueries
##  [!] See the following URL for more information:
##  https://conceptlibrary.saildatabank.com/api/v1/#operations-tag-phenotypes
search_results = client$phenotypes$get(
  ontology_code_descendants='251893009'
)

## Get a phenotype's version history
phenotype_versions = client$phenotypes$get_versions('PH1')

## Get details
phenotype_detail = client$phenotypes$get_detail('PH1')

## Get a specific version of the phenotype
phenotype_detail = client$phenotypes$get_detail('PH1', version_id=2)

## Get codelist
phenotype_codelist = client$phenotypes$get_codelist('PH1')

## Get a specific version of the phenotype
phenotype_codelist = client$phenotypes$get_codelist('PH1', version_id=2)


# Getting a phenotype's definition file

## Save a definition file
client$phenotypes$save_definition_file('./Examples/definition-files/PH1-definition-file.yaml', 'PH1')

## Save a definition file from a specific phenotype version
client$phenotypes$save_definition_file('./Examples/definition-files/PH1-definition-file.yaml', 'PH1', version_id=2)


# Creating/Updating phenotypes

## Create a phenotype from a definition file
result = client$phenotypes$create('./Examples/definition-files/example-phenotype.yaml')

## Update a phenotype from a definition file
result = client$phenotypes$update('./Examples/definition-files/example-phenotype.yaml')
