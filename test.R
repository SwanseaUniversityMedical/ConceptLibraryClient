library(ConceptLibraryClient)

client <- ConceptLibraryClient::connect_to_API(public=FALSE);

data <- ConceptLibraryClient::upload_phenotype(
  '[file-path]',
  client
);
