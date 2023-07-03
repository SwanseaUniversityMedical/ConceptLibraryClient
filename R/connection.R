Connection <- R6::R6Class(
  'ClientConnection',
  public = list(
    #' @field HttpClient (crul::HttpClient)
    HttpClient = NULL,

    #'
    initialize = function (username=NA, password=NA, public=FALSE, url=DEFAULT_CONNECTION_URL) {
      if (!public) {
        if (is.na(username) || is.na(password)) {
          message('Please log in to the Concept Library')
          auth_details = private$get_login(
            username=if (is.na(username)) '' else username
          )

          username = auth_details$username
          password = auth_details$password
        }

        self$HttpClient = crul::HttpClient$new(
          url,
          auth = crul::auth(user=username, pwd=password)
        )
      } else {
        self$HttpClient = crul::HttpClient$new(url)
      }
    }
  ),

  private = list(
    #' @description a
    is_authenticated = function () {
      return (length(self$HttpClient$auth) > 1)
    },

    #'
    get_login = function (username=NA) {
      login_window = tktoplevel()
      tkraise(login_window)

      tclVar(username) -> username_input
      tkgrid(tklabel(login_window, text='Username:'))
      tkgrid(tkentry(login_window, textvariable=username_input) -> password_box)

      tclVar('') -> password_input
      tkgrid(tklabel(login_window, text='Password'))
      tkgrid(tkentry(login_window, textvariable=password_input, show='*') -> password_box)

      tkbind(password_box, '<Return>', function () tkdestroy(login_window))
      tkgrid(tkbutton(login_window, text='Login', command=function() tkdestroy(login_window)))

      tkwait.window(login_window)

      return (list(
        username=tclvalue(username_input),
        password=tclvalue(password_input)
      ))
    }
  ),

  active = list(
    #'
    templates = function () {
      Templates$new(self$HttpClient)
    },

    #'
    phenotypes = function () {
      Phenotypes$new(self$HttpClient)
    },

    #'
    concepts = function () {
      Concepts$new(self$HttpClient)
    },

    #'
    collections = function () {
      Collections$new(self$HttpClient)
    },

    #'
    tags = function () {
      Tags$new(self$HttpClient)
    },

    #'
    datasources = function () {
      Datasources$new(self$HttpClient)
    }
  )
)
