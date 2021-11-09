library(GetoptLong)

# Set to TRUE to ignore tests on public API (i.e. inside gateway)
skip_public_API = FALSE

api_client = connect_to_API()
public_api_client = connect_to_public_API()
