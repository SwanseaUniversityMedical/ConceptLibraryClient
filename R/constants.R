#' DOMAINS
#'
#' @description
#' List containing default domains for connecting to the Concept Library
#'
#' @export
#'
DOMAINS = list(
  SAIL = 'https://conceptlibrary.saildatabank.com/',
  HDRUK = 'https://phenotypes.healthdatagateway.org/',
  ADP = 'https://conceptlibrary.saildatabank.com/ADP/',
  GATEWAY = 'http://conceptlibrary.serp.ac.uk/',
  DEMO = 'https://conceptlibrary.demo-dev.saildatabank.com/',
  LOCAL = 'http://127.0.0.1:8000/'
)

# Internal, default connection url to be used in Connection object
DEFAULT_CONNECTION_URL = DOMAINS$SAIL

# Internal, current API version
API_VERSION_NUMBER = 1

# Internal, prefix for API endpoints
API_PATH_PREFIX = sprintf('api/v%s/', API_VERSION_NUMBER)

# Interal, list of API endpoints
API_ENTITY_ENDPOINTS = list(
  TEMPLATES = list(
    INDEX = 'templates/',
    VERSION_HISTORY = 'templates/%s/get-versions/',
    DETAIL = 'templates/%s/detail/',
    DETAIL_BY_VERSION = 'templates/%s/version/%s/detail/'
  ),
  PHENOTYPES = list(
    INDEX = 'phenotypes/',
    VERSION_HISTORY = 'phenotypes/%s/get-versions/',
    DETAIL = 'phenotypes/%s/detail/',
    DETAIL_BY_VERSION = 'phenotypes/%s/version/%s/detail/',
    CODELIST = 'phenotypes/%s/export/codes/',
    CODELIST_BY_VERSION = 'phenotypes/%s/version/%s/export/codes/',
    CREATE = 'phenotypes/create/',
    UPDATE = 'phenotypes/update/'
  ),
  CONCEPTS = list(
    INDEX = 'concepts/',
    VERSION_HISTORY = 'concepts/%s/get-versions/',
    DETAIL = 'concepts/%s/detail/',
    DETAIL_BY_VERSION = 'concepts/%s/version/%s/detail/',
    CODELIST = 'concepts/%s/export/codes/',
    CODELIST_BY_VERSION = 'concepts/%s/version/%s/export/codes/'
  ),
  COLLECTIONS = list(
    INDEX = 'collections/',
    DETAIL = 'collections/%s/detail/'
  ),
  TAGS = list(
    INDEX = 'tags/',
    DETAIL = 'tags/%s/detail/'
  ),
  DATASOURCES = list(
    INDEX = 'data-sources/',
    DETAIL = 'data-sources/%s/detail/'
  )
)
