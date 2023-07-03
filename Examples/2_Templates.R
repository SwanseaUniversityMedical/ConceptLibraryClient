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


# Querying Phenotype templates

## Get all templates
template_list = client$templates$get()

## Get the version history of a template
template_versions = client$templates$get_versions(1)

## Get the template detail
template_detail = client$templates$get_detail(1)

## Get the template detail
template_detail = client$templates$get_detail(1, version_id=1)
