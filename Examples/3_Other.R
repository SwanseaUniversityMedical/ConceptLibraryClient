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
