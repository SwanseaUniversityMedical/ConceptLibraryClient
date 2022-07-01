API_URL <- "https://conceptlibrary.demo.saildatabank.com/"

API_YAML_TEMPLATE_VERSION <- 1
API_TEMPLATE_FIELDS <- list(
  "template_version",
  "phenotype_id",
  "phenotype_version_id",
  "concept_ids",
  "user"
)
API_CODING_SYSTEM_LOOKUP = list(
  'ICD-9'=17,
  'ICD-10'=4,
  'ICD-11'=18,
  'READ'=5,
  'OPCS'=7,
  'MED'=8,
  'SNOMED'=9,
  'PROD'=10,
  'BNF'=11,
  'UKBIOBANK'=12,
  'NON-STANDARD'=13,
  'CPRD'=14,
  'OXMIS'=15,
  'MULTILEX'=16
)

# Validation
API_PHENOTYPE_VALIDATION <- list(
  TYPES=c(
    'Disease or Syndrome',
    'Biomarker',
    'Lifestyle Risk Factor'
  ),
  CONCEPTS=c(
    'csv',
    'existing_concept',
    'inline'
  ),
  CODING_SYSTEM=c(
    'ICD-9',
    'ICD-10',
    'ICD-11',
    'READ',
    'OPCS',
    'MED',
    'SNOMED',
    'PROD',
    'BNF',
    'UKBIOBANK',
    'NON-STANDARD',
    'CPRD',
    'OXMIS',
    'MULTILEX'
  )
)

# API format
API_CODE_FORMAT = list(
  code="",
  description="",
  attributes=list()
)
API_COMPONENT_FORMAT=list(
  name="",
  comment="",
  component_type=4,
  logical_type=1,
  codes=list()
)
API_CONCEPT_FORMAT=list(
  name="",
  author="",
  description="",
  source_reference="",
  paper_published=FALSE,
  publication_link="",
  publication_doi="",
  secondary_publication_links="",
  citation_requirements="",
  validation_performed=FALSE,
  validation_description="",
  world_access=1,
  group_access=1,
  group=NA,
  tags=list(),
  coding_system=0,
  code_attribute_header=list(),
  components=list(),
  publish_immediately=FALSE
)
API_PHENOTYPE_FORMAT=list(
  phenotype_uuid="",
  title="",
  name="",
  author="",
  layout="Phenotype",
  type="",
  validation_performed=FALSE,
  validation="",
  valid_event_data_range="",
  sex="",
  status="FINAL",
  hdr_create_date="",
  hdr_modified_date="",
  paper_published=FALSE,
  publications="",
  publication_doi="",
  publication_link="",
  secondary_publication_links="",
  source_reference="",
  citation_requirements="",
  description="",
  implementation="",
  phenoflowid="",
  data_sources=list(),
  world_access=1,
  group_access=1,
  tags=list(),
  publish_immediately=FALSE
)
