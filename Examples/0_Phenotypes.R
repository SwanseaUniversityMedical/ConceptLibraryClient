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

## Search phenotypes
search_results = client$phenotypes$get(
  search='asthma',
  collections=19
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
client$phenotypes$save_to_file('./Examples/definition-files/PH1-definition-file.yaml', 'PH1')

## Save a definition file from a specific phenotype version
client$phenotypes$save_to_file('./Examples/definition-files/PH1-definition-file.yaml', 'PH1', version_id=2)


# Creating/Updating phenotypes

## Create a phenotype from a definition file
result = client$phenotypes$create('./Examples/definition-files/example-phenotype.yaml')

## Update a phenotype from a definition file
result = client$phenotypes$update('./Examples/definition-files/example-phenotype.yaml')
