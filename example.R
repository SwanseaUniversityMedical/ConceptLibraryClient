# Unload packages (if previous version of ConceptLibraryClient is loaded)
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

# Install from github
devtools::install_github('https://github.com/SwanseaUniversityMedical/ConceptLibraryClient')

# Load library
library(ConceptLibraryClient)

# Login
client <- ConceptLibraryClient::connect_to_API(
  user='my-conceptlibrary-username',
  url='https://conceptlibrary.demo.saildatabank.com/'
)

# Upload from .yaml file
results.upload <- ConceptLibraryClient::upload_phenotype(
  './path/to/file.yaml',
  client
)

# Update from .yaml file
results.update <- ConceptLibraryClient::update_phenotype(
  './path/to/file.yaml',
  client
)
