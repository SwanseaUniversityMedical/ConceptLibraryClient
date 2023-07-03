#
DOMAINS = list(
  SAIL = 'conceptlibrary.saildatabank.com/',
  HDRUK = 'phenotypes.healthdatagateway.org/',
  ADP = 'conceptlibrary.saildatabank.com/ADP/',
  GATEWAY = 'conceptlibrary.serp.ac.uk/',
  LOCAL = '127.0.0.1:8000/'
)

#
DEFAULT_CONNECTION_URL = DOMAINS$SAIL

#
API_VERSION_NUMBER = 1

#
API_PATH_PREFIX = sprintf('api/v%s/', API_VERSION_NUMBER)

#
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
    DETAIL = 'collections/%s/'
  ),
  TAGS = list(
    INDEX = 'tags/',
    DETAIL = 'tags/%s/'
  ),
  DATASOURCES = list(
    INDEX = 'data-sources/',
    DETAIL = 'data-sources/%s/'
  )
)
