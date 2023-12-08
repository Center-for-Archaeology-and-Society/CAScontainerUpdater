# app/view/login

box::use(
  shiny[tabPanel, div, HTML, actionButton, uiOutput,moduleServer,observeEvent,showModal,modalDialog,textInput,passwordInput,tagList,modalButton,removeModal,reactive,renderUI,req,renderText,NS],
  shinyauthr[logoutUI, loginUI,loginServer,logoutServer],
  sodium[password_store],
  tibble[tibble],
  dplyr[bind_rows],
  DBI[dbWriteTable]
)

box::use(
  app/logic/myNotification[myNotification],
)

#' @export
ui = function(id){
  ns = NS(id)
  tabPanel(
    title = "Login",
    value = "login",
    # add logout button UI
    div(class = "pull-right", logoutUI(id = ns("logout"))),
    div(
      HTML(
        "<h3>Forgot password? Email <a href=\"mailto:rbischoff@asu.edu\">rbischoff@asu.edu</a></h3>
"
      ),
id = ns("forgotPassword")
    ),
actionButton(ns("create_user"), "Create user"),
# add login panel UI function
loginUI(ns("login")),
uiOutput(ns("welcomeUI"))
  )
}

#' @export
server = function(id,id2,database,con2){
  moduleServer(id, function(input, output, session){
    ns = NS(paste0(id2,"-",id))

    observeEvent(input$create_user, {
      message("creating user")
      showModal(
        modalDialog(
          title = "New user information",
          textInput(ns("new_username"), "Username:"),
          textInput(ns("new_name"), "Name:"),
          passwordInput(ns("new_password1"), "Password:"),
          passwordInput(ns("new_password2"), "Confirm password:"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_user"), "Save",
                         class = "btn-primary")
          ),
          easyClose = TRUE,
          size = "m",
          closeOnEscape = TRUE,
          closeOnClickOutside = TRUE
        )
      )
    })

    observeEvent(input$save_user, {
      message("saving user")
      # Get the values of the input fields
      username <- input$new_username
      password1 <- input$new_password1
      password2 <- input$new_password2
      name <- input$new_name

      # Validate the passwords match
      if (username %in% database$user) {
        myNotification("Username already exists. Please try again.",
                       type = "error")
        return()
      }

      if (password1 != password2) {
        myNotification("Passwords don't match. Please try again.",
                       type = "error")
        return()
      }

      new = tibble(
        user = username,
        password = password_store(password1),
        permissions = "standard",
        name = name
      )

      dbWriteTable(con2,"user_data",new,append = TRUE)

      myNotification(sprintf("User '%s' was created successfully!\n", new$user))
      removeModal()
      session$reload()

    })

    credentials <- loginServer(
      id = "login",
      data = database,
      user_col = user,
      pwd_col = password,
      sodium_hashed = T,
      log_out = reactive(logout_init())
    )

    logout_init <- logoutServer(id = "logout",
                                active = reactive(credentials()$user_auth))

    output$welcomeUI = renderUI({
      req(credentials()$user_auth)
      renderText(paste("Welcome", credentials()$info$name))
    })

    return(credentials)

  })
}
