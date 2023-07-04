#' ConceptLibraryClient: A Concept Library API Client
#'
#' @description
#' This package acts as a client for the ConceptLibrary API
#'
#' @section Authenticated API connection:
#' By calling \code{ConceptLibraryClient::Connection$new()}, the user must
#'  provide login credentials either in the function itself, or in the popup
#'  window if none are given.
#'
#' @section Non-authenticated API connection:
#' A non-authenticated, or public, connection can be made to the ConceptLibrary
#'  by calling \code{ConceptLibraryClient::Connection$new(public=TRUE)}.
#'
#' @section Modifying API connection URL:
#' The connection function also provides a \code{url} parameter to specify a
#'  different URL to send the API requests to.
#'
#' We provide default connection urls through:
#'  \code{ConceptLibraryClient::DOMAINS}
#'
#' @section Making API requests:
#' The following functions can be used to retrieve the various types of data
#'  stored in the Concept Library.
#'
#' ## Phenotypes
#' * \code{client$phenotypes$get}
#' * \code{client$phenotypes$get_versions}
#' * \code{client$phenotypes$get_detail}
#' * \code{client$phenotypes$get_codelist}
#' * \code{client$phenotypes$save_to_file}
#' * \code{client$phenotypes$create}
#' * \code{client$phenotypes$update}
#'
#' ## Concepts
#' * \code{client$concepts$get}
#' * \code{client$concepts$get_versions}
#' * \code{client$concepts$get_detail}
#' * \code{client$concepts$get_codelist}
#'
#' ## Templates
#' * \code{client$templates$get}
#' * \code{client$templates$get_versions}
#' * \code{client$templates$get_detail}
#'
#' ## Collections
#' * \code{client$templates$get}
#' * \code{client$templates$get_detail}
#'
#' ## Tags
#' * \code{client$templates$get}
#' * \code{client$templates$get_detail}
#'
#' ## Datasources
#' * \code{client$templates$get}
#' * \code{client$templates$get_detail}
#'
#' @md
#' @docType package
#' @name ConceptLibraryClient
#'
#' @import R6
#' @import crul
#' @import tcltk
NULL
