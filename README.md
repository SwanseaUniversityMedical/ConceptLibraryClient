# ConceptLibraryClient
An implementation of the API client for the Concept Library in R.

The Concept Library is a system for storing, managing, sharing, and documenting clinical code lists in health research. 
More information can be found here: [https://conceptlibrary.saildatabank.com/](https://conceptlibrary.saildatabank.com/).

## Installation
This package can be easily installed using `install_github`. In your R console, type the following commands:
```
library(devtools)
install_github("SwanseaUniversityMedical/ConceptLibraryClient")
```

## Using the package
The package provides a function to connect to the Concept Library API and multiple functions to send requests to the API's
endpoints.

### Creating an API connection
The Concept Library uses two types of connection for its API: a public connection and an authenticated connection.
The endpoints you can use and the data they return depends on the connection type that is used.

The package provides the `connect_to_API()` function to create an API connection. The `public` parameter sets the connection type.
By default this is set to `TRUE`, so to create an authenticated connection use `public = FALSE`.
An authenticated connection requires login credentials. These can be provided with the `user` and `password` paramaters, but a login window
also appears if either of those is left blank. *Please note this window sometimes appears behind other windows.*

The `url` parameter can be used a specify a different version of the Concept Library API.

The function creates a connection object which can be passed to the API endpoint functions. For most functions however, this is not a requirement.
If the connection object is not specified then a public connection is automatically created. This is most useful when calling a singular API endpoint.

### Making API requests
The following functions can be used to retrieve the various types of data stored in the Concept Library. For each function, more information can be found
using the `?` command in the R console. E.g. to find out more about the `get_phenotypes()` function, type `?ConceptLibraryClient::get_phenotypes`.
More information on the API can also be found [here](https://conceptlibrary.saildatabank.com/api/v1/).

#### Phenotypes
- `get_phenotypes()`
- `get_phenotype_by_id()`
- `get_phenotype_detail()`
- `get_phenotype_detail_by_version()`
- `get_phenotype_code_list()`
- `get_phenotype_versions()`
- `upload_phenotype()`
- `save_phenotype_definition()`

#### Concepts
- `get_concepts()`
- `get_concept_by_id()`
- `get_concept_detail()`
- `get_concept_detail_by_version()`
- `get_concept_code_list()`
- `get_concept_code_list_by_version()`
- `get_concept_versions()`

#### Data Sources
- `get_data_sources()`
- `get_data_source_by_id()`

#### Tags
- `get_tags()`
- `get_tag_by_id()`

#### Collections
- `get_collections()`
- `get_collection_by_id()`

#### Working Sets *(Deprecated)*
The package provides functions for working sets, but please note these will soon be removed.
- `get_working_sets()`
- `get_working_set_by_id()`
- `get_working_set_detail()`
- `get_working_set_detail_by_version()`
- `get_working_set_code_list()`
- `get_working_set_code_list_by_version()`
- `get_working_set_versions()`
