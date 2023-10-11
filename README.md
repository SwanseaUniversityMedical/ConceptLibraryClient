# ConceptLibraryClient
An implementation of the API client for the Concept Library in R.

The Concept Library is a system for storing, managing, sharing, and documenting clinical code lists in health research. 
More information can be found here: [https://conceptlibrary.saildatabank.com/](https://conceptlibrary.saildatabank.com/).

# Installation
This package can be easily installed using `install_github`. In your R console, type the following commands:
``` R
library(devtools)
devtools::install_github("SwanseaUniversityMedical/ConceptLibraryClient")
```

# Using the package
The package provides a function to connect to the Concept Library API and multiple functions to send requests to the API's
endpoints.

## Creating an API connection
The Concept Library uses two types of connection for its API: a public connection and an authenticated connection.
The endpoints you can use and the data they return depends on the connection type that is used.

``` R
# Non-authenticated API
client = ConceptLibraryClient::Connection$new(
  public=TRUE
)

# Authenticated API (login through tkinter window)
ConceptLibraryClient::Connection$new(
  public=FALSE
)

# Authenticated API (login with supplied credentials)
ConceptLibraryClient::Connection$new(
  username='my-username',
  password='my-password'
)
```

The `url` parameter can be used a specify a different version of the Concept Library API.

``` R
# Using common domains stored in ConceptLibraryClient::DOMAINS
## SAIL (conceptlibrary.saildatabank.com/)
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$SAIL
)

## HDRUK (phenotypes.healthdatagateway.org/)
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$HDRUK
)

## ADP (conceptlibrary.saildatabank.com/ADP/)
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$ADP
)

## Gateway (conceptlibrary.serp.ac.uk/)
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$GATEWAY
)

## Demo site (conceptlibrary.demo-dev.saildatabank.com/)
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url=ConceptLibraryClient::DOMAINS$DEMO
)

# Custom URL
client = ConceptLibraryClient::Connection$new(
  public=TRUE,
  url='my-custom-url.com/'
)
```

The function creates a connection object which can be used to query the ConceptLibrary.

## Making API requests
The following functions can be used to retrieve the various types of data stored in the Concept Library. For each function, more information can be found using the `?` command in the R console.

More information on the API can also be found [here](https://conceptlibrary.saildatabank.com/api/v1/).

### Templates
#### Help
Run `?ConceptLibraryClient::Templates` in the console to view the documentation

### Querying
``` R
# Get all templates
template_list = client$templates$get()

# Get the version history of a template
template_versions = client$templates$get_versions(1)

# Get the template detail
template_detail = client$templates$get_detail(1)

# Get the template detail
template_detail = client$templates$get_detail(1, version_id=1)
```

### Phenotypes
#### Help
Run `?ConceptLibraryClient::Phenotypes` in the console to view the documentation

#### Querying
``` R
# Search phenotypes
search_results = client$phenotypes$get(
  search='asthma',
  collections=19
)

# Get a phenotype's version history
phenotype_versions = client$phenotypes$get_versions('PH1')

# Get details
phenotype_detail = client$phenotypes$get_detail('PH1')

# Get a specific version of the phenotype
phenotype_detail = client$phenotypes$get_detail('PH1', version_id=2)

# Get codelist
phenotype_codelist = client$phenotypes$get_codelist('PH1')

# Get a specific version of the phenotype
phenotype_codelist = client$phenotypes$get_codelist('PH1', version_id=2)
```

#### Downloading a definition file
``` R
# Save a definition file
client$phenotypes$save_definition_file('./Examples/definition-files/PH1-definition-file.yaml', 'PH1')

# Save a definition file from a specific phenotype version
client$phenotypes$save_definition_file('./Examples/definition-files/PH1-definition-file.yaml', 'PH1', version_id=2)
```

#### Creating/Updating
``` R
# Create a phenotype from a definition file
result = client$phenotypes$create('./Examples/definition-files/example-phenotype.yaml')

# Update a phenotype from a definition file
result = client$phenotypes$update('./Examples/definition-files/example-phenotype.yaml')
```

### Concepts
#### Help
Run `?ConceptLibraryClient::Concepts` in the console to view the documentation

#### Querying
``` R
# Search concepts
search_results = client$concepts$get(
  search='asthma',
  collections=19
)

# Get a concepts's version history
concept_versions = client$concepts$get_versions('C714')

# Get details
concept_detail = client$concepts$get_detail('C714')

# Get the details of a specific version of the concept
concept_detail = client$concepts$get_detail('C714', version_id=2567)

# Get codelist
concept_codelist = client$concepts$get_codelist('C714')

# Get the codelist of a specific version of the concept
concept_codelist = client$concepts$get_codelist('C714', version_id=2567)
```

### Collections
#### Help
Run `?ConceptLibraryClient::Phenotypes` in the console to view the documentation

#### Querying
``` R
# Get all collections
collection_list = client$collections$get()

# Get the collections detail
collection_detail = client$collections$get_detail(18)
```

### Tags
#### Help
Run `?ConceptLibraryClient::Phenotypes` in the console to view the documentation

#### Querying
``` R
# Get all tags
tag_list = client$tags$get()

# Get the tags detail
tag_detail = client$tags$get_detail(1)
```

### Datasources
#### Help
Run `?ConceptLibraryClient::Datasources` in the console to view the documentation

#### Querying
``` R
# Get all datasources
datasource_list = client$datasources$get()

# Get the datasources detail
datasource_detail = client$datasources$get_detail(1)
```
