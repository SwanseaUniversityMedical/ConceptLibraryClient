#' Connection
#'
#' @description
#' Connection object, storing HttpClient connection and allowing access to
#'  endpoint instances, e.g. templates, phenotypes
#'
#' @examples
#' # Authenticated login, with url set to HDRUK
#' client = ConceptLibraryClient::Connection$new(
#'   url=ConceptLibraryClient::DOMAINS$HDRUK
#' )
#'
#' # Authenticated login, with custom URL
#' client = ConceptLibraryClient::Connection$new(
#'   url='custom-url.com/'
#' )
#'
#' # Non-authenticated connection
#' client = ConceptLibraryClient::Connection$new(public=FALSE)
#'
#' # Accessing phenotypes
#' phenotype_detail = client$phenotypes$get('PH1', version_id=2)
#'
#' @export
#'
Connection <- R6::R6Class(
  'ClientConnection',
  public = list(
    #' @description
    #' Create a connection to the ConceptLibrary
    #'
    #' @param username (string) Username for authentication
    #' @param password (string) Password for authentication
    #' @param public (bool) Flag to determine whether login is required
    #' @param url (string) URL used when connecting to the ConceptLibrary
    #'
    initialize = function (
      username=NA, password=NA, public=FALSE, url=DEFAULT_CONNECTION_URL
    ) {
      if (!public) {
        if (is.na(username) || is.na(password)) {
          message('Please log in to the Concept Library')
          auth_details = private$get_login(
            username=if (is.na(username)) '' else username
          )

          username = auth_details$username
          password = auth_details$password
        }

        private$HttpClient = crul::HttpClient$new(
          url,
          auth = crul::auth(user=username, pwd=password)
        )
      } else {
        private$HttpClient = crul::HttpClient$new(url)
      }
    }
  ),

  private = list(
    #' @field HttpClient (crul::HttpClient) Connection object
    HttpClient = NULL,

    #' @description
    #' Checks whether the connection is authenticated
    #'
    #' @returns TRUE if authenticated, FALSE otherwise
    #'
    is_authenticated = function () {
      return (length(self$HttpClient$auth) > 1)
    },

    #' @description
    #' Creates login box for the user to input authentication details
    #'
    #' @param username (string) Username to populate username field, defaults to empty string
    #'
    #' @returns List containing user inputted username and password
    #'
    get_login = function (username='') {
      login_window = tktoplevel()
      tkraise(login_window)

      tclVar(username) -> username_input
      tkgrid(tklabel(login_window, text='Username:'))
      tkgrid(tkentry(login_window, textvariable=username_input) -> password_box)

      tclVar('') -> password_input
      tkgrid(tklabel(login_window, text='Password'))
      tkgrid(tkentry(
        login_window, textvariable=password_input, show='*'
      ) -> password_box)

      tkbind(password_box, '<Return>', function () tkdestroy(login_window))
      tkgrid(tkbutton(
        login_window, text='Login', command=function() tkdestroy(login_window)
      ))

      tkwait.window(login_window)

      return (list(
        username=tclvalue(username_input),
        password=tclvalue(password_input)
      ))
    }
  ),

  active = list(
    #' @field templates (ConceptLibraryClient::Templates) Templates instance
    templates = function () Templates$new(private$HttpClient),

    #' @field phenotypes (ConceptLibraryClient::Phenotypes) Phenotypes instance
    phenotypes = function () Phenotypes$new(private$HttpClient),

    #' @field concepts (ConceptLibraryClient::Concepts) Concepts instance
    concepts = function () Concepts$new(private$HttpClient),

    #' @field collections (ConceptLibraryClient::Collections) Collections instance
    collections = function () Collections$new(private$HttpClient),

    #' @field tags (ConceptLibraryClient::Tags) Tags instance
    tags = function () Tags$new(private$HttpClient),

    #' @field datasources (ConceptLibraryClient::Datasources) Datasources instance
    datasources = function () Datasources$new(private$HttpClient)
  )
)
