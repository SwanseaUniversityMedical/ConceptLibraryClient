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

## Search concepts
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
