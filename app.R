library(shiny)
library(magrittr)
library(dplyr)
library(DT)
library(shinyjs)
library(janitor)
library(readr)
library(stringr)
library(tidyr)
library(rio)
library(ssh)
library(DBI)
library(dbplyr)
library(RMySQL)

con = dbConnect(MySQL(),
                host = "10.126.24.122",
                dbname = "ca",
                user = Sys.getenv("uid"),
                password = Sys.getenv("pwd"),
                port = 3306
)

dir = "/mnt/storage/public"

sshSession = ssh_connect("ca@10.126.24.122", passwd = Sys.getenv("pwd"))

safeSaveRDS = function(object,file){
  if(file.access(file, mode = 2) == 0){
    try(saveRDS(object,file))
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch(ssh_exec_wait(sshSession, command = glue::glue('sudo -S {Sys.getenv("pwd")} chmod +777 {file}')),error = function(e) warning(glue::glue("unable to save {file}")))
    }
    try(saveRDS(object,file))
  }
}

safeImport = function(file, ...){
  if(file.access(file, mode = 4) == 0){
    object = tryCatch(rio::import(file, setclass = 'tibble', ...), error =  function(e) return(NULL))
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch(ssh_exec_wait(sshSession, command = glue::glue('sudo -S {Sys.getenv("pwd")} chmod +777 {file}')),error = function(e) {
        warning(glue::glue("unable to read {file}"))
        return(NULL)
      })
    }
    object = tryCatch(rio::import(file, setclass = 'tibble' ,...), error =  function(e) {
      warning(glue::glue("unable to read {file}"))
      return(NULL)
    })
  }
  return(object)
}

storage2idno = function(df){
  df %>%
    mutate(buildingid = case_when(!is.na(building)~paste0("b",building)),
           roomid = case_when(!is.na(room)~paste0("rm",room)),
           rowid = case_when(!is.na(row)~paste0("rw",row)),
           unitid = case_when(!is.na(unit)~paste0("u",unit)),
           shelfid = case_when(!is.na(shelf)~paste0("s",shelf)),
           buildingid = str_replace_all(buildingid,"Alameda","Ala"),
           shelfid = str_replace_all(shelfid,"floor","fl"),
           unitid = str_replace_all(unitid,"in front of","f"),
           unitid = str_replace_all(unitid,"across from","a"),
           unitid = str_replace_all(unitid,"Top of filing cabinets","tofc"),
           rowid = str_replace_all(rowid,"floor","fl")) %>%
    select(-any_of(c("idno","barcode"))) %>%
    unite("idno",any_of(contains("barcode")),sep = "_",remove = T, na.rm = T) %>%
    mutate(date = date %>% as.integer %>% as.Date(origin = "1899-12-30"),
           entered = T) %>%
    mutate(idno= str_replace_all(idno," ","_")) %>%
    rowwise() %>%
    mutate_at(vars(idno),list(function(c) {
      if(nchar(c) > 30) str_sub(c,nchar(c)-29,nchar(c)) else c
    })) %>%
    ungroup()
}

idno2storage = function(df){
  if(!"idno" %in% names(df)) stop("No idno in storage locations")
  seps = c("_s", "_u", "_rw", "_rm")
  ids = c("shelf","unit","row","room")
  df %<>%
    mutate(idnoTmp = idno)
  for(i in 1:length(seps)){
    df %<>%
      separate(idnoTmp, into = c('idnoTmp', ids[i]),sep = seps[i], fill = "right")
  }
  df %<>%
    mutate(idnoTmp = idnoTmp %>% str_remove_all("^b")) %>%
    rename(building = idnoTmp)
  return(df)
}

buildings = dplyr::tibble(building = c("Ala", "CSB", "MC", "MH"), fullBuilding = c("Alameda",
                                                                                   "CSB", "MC", "MH"))
ui <- navbarPage(
  title = "CASR Box Updater",

  id = "page",

  useShinyjs(),

  tabPanel("login",
           # add logout button UI
           div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
           div(HTML("<h3>Forgot password? Email <a href=\"mailto:rbischoff@asu.edu\">rbischoff@asu.edu</a></h3>
"),id = "forgotPassword"),
actionButton("create_user", "Create user"),
# add login panel UI function
shinyauthr::loginUI(id = "login"),
uiOutput("welcomeUI")
  ),
tabPanel("main",
         wellPanel(
           fluidRow(column(width = 3,textInput("boxno","boxno"))),
           fluidRow(
             column(width = 2,textInput("barcode","barcode")),
             column(width = 2,textInput("idno","idno")),
             column(width = 2,selectInput("building","building",choices = c("",buildings$building) %>% setNames(c("",buildings$fullBuilding)))),
             column(width = 2,textInput("room","room")),
             column(width = 1,textInput("row","row")),
             column(width = 1,textInput("unit","unit")),
             column(width = 2,textInput("shelf","shelf"))
           ),
           fluidRow(
             column(width = 3,textInput("person","Person(s) responsible")),
             column(width = 3,selectInput("type","type of move", choices = c("permanent","temporary","returned"))),
             column(width = 3,textInput("purpose","purpose of transfer")),
             column(width = 3,dateInput("date","date")),
           ),
           fluidRow(
             column(width = 3, actionButton(inputId = "add","add"))
           )
         ),
         fluidRow(column(width = 3,wellPanel(actionButton("deleteRow","Delete selection")))),
         DT::DTOutput(outputId = "table"),
         actionButton(inputId = "submit",label = "submit batch")
),
tabPanel(title = "storage locations",value = "storageLocations",
         DT::DTOutput(outputId = "storageTable")
)
)
server <- function(input, output, session) {

  # make sure directory is readable

  observeEvent(input$add,{
    ssh_exec_wait(sshSession, command = 'sudo chmod -R +777 /mnt/storage/public')
  })

  rvals = reactiveValues(df = tibble::tibble(),
                         storageLocations = dbGetQuery(con,"select ca_storage_locations.location_id, ca_storage_locations.idno, value_longtext1 as barcode from ca_storage_locations join ca_attributes on ca_attributes.row_id = ca_storage_locations.location_id join ca_attribute_values on ca_attribute_values.attribute_id = ca_attributes.attribute_id join ca_metadata_elements on ca_metadata_elements.element_id = ca_attributes.element_id where element_code = 'barcode' and ca_storage_locations.deleted = 0") %>% idno2storage())

  database = safeImport("database.Rds")

  observeEvent(input$create_user, {
    showModal(modalDialog(
      title = "New user information",
      textInput("new_username", "Username:"),
      textInput("new_name", "Name:"),
      passwordInput("new_password1", "Password:"),
      passwordInput("new_password2", "Confirm password:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_user", "Save",
                     class = "btn-primary")
      ),
      easyClose = TRUE,
      size = "m",
      closeOnEscape = TRUE,
      closeOnClickOutside = TRUE
    ))
  })

  observeEvent(input$save_user, {
    # Get the values of the input fields
    username <- input$new_username
    password1 <- input$new_password1
    password2 <- input$new_password2
    name <- input$new_name

    # Validate the passwords match
    if(username %in% database$user){
      showNotification("Username already exists. Please try again.",
                       type = "error")
      return()
    }

    if (password1 != password2) {
      showNotification("Passwords don't match. Please try again.",
                       type = "error")
      return()
    }

    new = tibble(user = username, password = sodium::password_store(password1), permissions = "standard",name = name)

    databaseNew = bind_rows(database,new)
    safeSaveRDS(databaseNew,"database.Rds")

    showNotification(sprintf("User '%s' was created successfully!\n", new$user))
    removeModal()
    session$reload()

  })

  # login ----

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = database,
    user_col = user,
    pwd_col = password,
    sodium_hashed = T,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  output$welcomeUI = renderUI({
    req(credentials()$user_auth)
    renderText(paste("Welcome",credentials()$info$name))
  })

  observeEvent(credentials()$user_auth,{
    if(credentials()$user_auth) {
      showTab("page","main")
    } else {
      hideTab("page","main")
    }
  })

  observeEvent(credentials(),{
    req(credentials()$user_auth)
    updateTextInput(session = session,inputId = "person",value = credentials()$info$name)
  })

  observeEvent(input$add,{
    req(input$type)
    requiredCols = c("purpose" = NA_character_,"person" = NA_character_,"date" = NA_character_)
    inputdf = tibble(
      boxno = input$boxno,
      barcode = input$barcode,
      idno = input$idno,
      building = input$building,
      room = input$room,
      row = input$row,
      unit = input$unit,
      shelf = input$shelf,
      type = input$type,
      purpose = input$purpose,
      person = input$person,
      date = coalesce(input$date,NA)
    ) %>%
      mutate_at(vars(-date),str_trim) %>%
      mutate_at(vars(-date),na_if,"") %>%
      mutate_at(vars(-date),as.character) %>%
      remove_empty("cols") %>%
      tibble::add_column(!!!requiredCols[!names(requiredCols) %in% names(.)])
    print(inputdf)
    print(dput(inputdf))
    inputdf %<>%
      inner_join(
        rvals$storageLocations
      )


    rvals$df = bind_rows(inputdf,rvals$df) %>%
      distinct_all() %>%
      select(boxno,barcode, location_id, idno, type, purpose, person, date, building, room, row, unit, shelf)
  })

  observeEvent(input$submit, {
    showNotification("submitting batch")
    tmpdir = tempdir()
    rio::export(rvals$df,file.path(tmpdir,"importBoxMoves.xlsx"))
    ssh::scp_upload(session = sshSession,files = file.path(tmpdir,"importBoxMoves.xlsx"),to = file.path(dir,"importBoxMoves.xlsx"))
    rvals$df = tibble::tibble()
    ssh_exec_wait(sshSession, command = paste0(dir,'/boxtransferauto.sh'))
    file.remove(file.path(tmpdir,"importBoxMoves.xlsx"))
    showNotification("completed")
  })

  output$table = renderDT({
    DT::datatable(rvals$df,rownames = F)
  })

  observeEvent(input$deleteRow,{

    indx = input$table_rows_selected

    if(length(indx) > 0){
      rvals$df <- tryCatch(rvals$df %>%
                             slice(-indx),error = function(e) return(rvals$df))
    }
  })

  output$storageTable = DT::renderDT({
    DT::datatable(rvals$storageLocations, rownames = F,filter = "top")
  })
}

shinyApp(ui = ui, server = server)
