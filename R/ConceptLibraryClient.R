#' ConceptLibraryClient: A Concept Library API client.
#'
#' This package acts as a client for the Concept Library API. Each function represents an HTTP request to the API to
#' interact with Phenotypes, Concepts, Data Sources, Working Sets and Tags.
#'
#' Before using these functions, the package user can create an HttpClient object by calling the connect_to_API
#' function. Once this object is created, it can be passed into any API function that is called, which avoids having to
#' create a new connection each time when more than one API endpoint is to be used.
#'
#' Most functions can be used without specifying a connection object. This will automatically create a public API
#' connection, which is useful when sending a singular request.
#'
#' @section Authenticated API connection:
#' By calling \code{\link{connect_to_API}} with \code{public = FALSE}, the user must provide login credentials either in
#' the function itself, or in the popup window if none are given.
#'
#' This connection can then be used with any of the API functions available
#'
#' @section Public API connection:
#' \code{\link{connect_to_API}} also allows for a connection without requiring authentication with \code{public = TRUE}.
#' This is the default connection method, so calling \code{connect_to_API()} will have the same effect. Please note
#' some functions require an authenticated connection.
#'
#' The connection function also provides a \code{url} parameter to specify a different URL to send the API requests to.
#'
#' @section Making API requests:
#' The following functions can be used to retrieve the various types of data stored in the Concept Library.
#'
#' ## Phenotypes
#' * \code{\link{get_phenotypes}}
#'
#' @md
#' @docType package
#' @name ConceptLibraryClient
#'
#' @import R6
#' @import crul
#' @import tcltk
NULL
