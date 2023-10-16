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
library(readxl)
library(ssh)
library(DBI)
library(dbplyr)
library(RMySQL)

con = dbConnect(
  MySQL(),
  host = "10.126.24.122",
  dbname = "ca",
  user = Sys.getenv("uid"),
  password = Sys.getenv("pwd"),
  port = 3306
)

dir = "/mnt/storage/public"

sshSession = ssh_connect("ca@10.126.24.122", passwd = Sys.getenv("pwd"))

safeSaveRDS = function(object, file) {
  if (file.access(file, mode = 2) == 0) {
    try(saveRDS(object, file))
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch(
        ssh_exec_wait(
          sshSession,
          command = glue::glue('sudo -S {Sys.getenv("pwd")} chmod +777 {file}')
        ),
        error = function(e) {
          msg = glue::glue("unable to save {file}: {e}")
          warning(msg)
          myNotification(msg, type = 'warning')
        }
      )
    }
    try(saveRDS(object, file))
  }
}

safeImport = function(file, ...) {
  if (file.access(file, mode = 4) == 0) {
    object = tryCatch(
      rio::import(file, setclass = 'tibble', ...),
      error =  function(e)
        return(NULL)
    )
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch(
        ssh_exec_wait(
          sshSession,
          command = glue::glue('sudo -S {Sys.getenv("pwd")} chmod +777 {file}')
        ),
        error = function(e) {
          msg = glue::glue("unable to read {file}: {e}")
          warning(msg)
          myNotification(msg, type = 'warning')
          return(NULL)
        }
      )
    }
    object = tryCatch(
      rio::import(file, setclass = 'tibble' , ...),
      error =  function(e) {
        msg = glue::glue("unable to read {file}: {e}")
        warning(msg)
        myNotification(msg, type = 'warning')
        return(NULL)
      }
    )
  }
  return(object)
}

storage2idno = function(df) {
  df %>%
    mutate(
      buildingid = case_when(!is.na(building) ~ paste0("b", building)),
      roomid = case_when(!is.na(room) ~ paste0("rm", room)),
      rowid = case_when(!is.na(row) ~ paste0("rw", row)),
      unitid = case_when(!is.na(unit) ~ paste0("u", unit)),
      shelfid = case_when(!is.na(shelf) ~ paste0("s", shelf)),
      shelfid = str_replace_all(shelfid, "floor", "fl"),
      unitid = str_replace_all(unitid, "in front of", "f"),
      unitid = str_replace_all(unitid, "across from", "a"),
      unitid = str_replace_all(unitid, "Top of filing cabinets", "tofc"),
      rowid = str_replace_all(rowid, "floor", "fl")
    ) %>%
    select(-any_of('idno')) %>%
    unite(
      'idno',
      c(buildingid, roomid, rowid, unitid, shelfid),
      sep = "_",
      na.rm = T,
      remove = T
    ) %>%
    mutate(idno = str_replace_all(idno, " ", "_")) %>%
    rowwise() %>%
    mutate_at(vars(idno), list(function(c) {
      if (nchar(c) > 30)
        str_sub(c, nchar(c) - 29, nchar(c))
      else
        c
    })) %>%
    ungroup()
}

idno2storage = function(df) {
  if (!"idno" %in% names(df))
    stop("No idno in storage locations")
  seps = c("_s", "_u", "_rw", "_rm")
  ids = c("shelf", "unit", "row", "room")
  df %<>%
    mutate(idnoTmp = idno)
  for (i in 1:length(seps)) {
    df %<>%
      separate(
        idnoTmp,
        into = c('idnoTmp', ids[i]),
        sep = seps[i],
        fill = "right"
      )
  }
  df %<>%
    mutate(idnoTmp = idnoTmp %>% str_remove_all("^b")) %>%
    rename(building = idnoTmp)
  return(df)
}

getParent = function(x) {
  seps = c("_s", "_u", "rw", "rm")
  for (s in seps) {
    if (stringr::str_detect(x, s)) {
      split = x %>% str_split_1(pattern = s)
      split = split[-length(split)]
      break
    } else
      split = ""
  }
  split = stringr::str_remove_all(split, "_$")
  return(split)
}

myNotification = function(msg, type = 'default') {
  if (type %in% c("warning", "error")) {
    warning(msg)
  } else {
    print(msg)
  }
  try(showNotification(msg, type = type), silent = T)
}

ui <- navbarPage(
  title = "CASR Box Updater",

  id = "page",

  useShinyjs(),

  tabPanel(
    "login",
    # add logout button UI
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    div(
      HTML(
        "<h3>Forgot password? Email <a href=\"mailto:rbischoff@asu.edu\">rbischoff@asu.edu</a></h3>
"
      ),
id = "forgotPassword"
    ),
actionButton("create_user", "Create user"),
# add login panel UI function
shinyauthr::loginUI(id = "login"),
uiOutput("welcomeUI")
  ),
tabPanel(
  "main",
  wellPanel(
    fluidRow(column(width = 3, textInput("boxno", "boxno"))),
    fluidRow(
      column(width = 2, textInput("barcode", "barcode")),
      column(width = 2, textInput("idno", "idno")),
      column(width = 2, uiOutput('buildingUI')),
      column(width = 2, textInput("room", "room")),
      column(width = 1, textInput("row", "row")),
      column(width = 1, textInput("unit", "unit")),
      column(width = 2, textInput("shelf", "shelf"))
    ),
    fluidRow(
      column(width = 3, textInput("person", "Person(s) responsible")),
      column(width = 3, selectInput(
        "type",
        "type of move",
        choices = c("permanent", "temporary", "returned")
      )),
      column(width = 3, textInput("purpose", "purpose of transfer")),
      column(width = 3, dateInput("date", "date")),
    ),
    fluidRow(column(
      width = 3, actionButton(inputId = "add", "add")
    ))
  ),
  fluidRow(column(width = 3, wellPanel(
    actionButton("deleteRow", "Delete selection")
  ))),
  DT::DTOutput(outputId = "table"),
  actionButton(inputId = "submit", label = "submit batch")
),
tabPanel(
  title = "storage locations",
  value = "storageLocations",
  DT::DTOutput(outputId = "storageTable")
),
tabPanel(
  title = "upload new storage locations",
  value = 'newstoragelocations',
  h3('instructions'),
  p(
    "Upload an excel file with columns having these exact names: building, room, row, unit, shelf. Fill out the information for each storage location as appropriate. Remember to spell the names exactly as spelled in the database: e.g., CSB not Community Services Building, Alameda not Ala,  B not b, etc. Next, upload the spreadsheet using the import button and press go. Wait for the confirmation notification to confirm it worked."
  ),
  fileInput("newLocationsFile", "import", accept = c(".xlsx")),
  DTOutput('storageUploadDT'),
  actionButton("newLocationsGo", "go")
)
)
server <- function(input, output, session) {
  # make sure directory is readable

  observeEvent(input$add, {
    ssh_exec_wait(sshSession, command = 'sudo chmod -R +777 /mnt/storage/public')
  })

  rvals = reactiveValues(
    df = tibble::tibble(),
    storageLocations = dbGetQuery(
      con,
      "select ca_storage_locations.location_id, ca_storage_locations.idno, value_longtext1 as barcode from ca_storage_locations join ca_attributes on ca_attributes.row_id = ca_storage_locations.location_id join ca_attribute_values on ca_attribute_values.attribute_id = ca_attributes.attribute_id join ca_metadata_elements on ca_metadata_elements.element_id = ca_attributes.element_id where element_code = 'barcode' and ca_storage_locations.deleted = 0"
    ) %>% idno2storage()
  )

  database = safeImport("database.Rds")

  observeEvent(input$create_user, {
    showModal(
      modalDialog(
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
      )
    )
  })

  observeEvent(input$save_user, {
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
      password = sodium::password_store(password1),
      permissions = "standard",
      name = name
    )

    databaseNew = bind_rows(database, new)
    safeSaveRDS(databaseNew, "database.Rds")

    myNotification(sprintf("User '%s' was created successfully!\n", new$user))
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
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))

  output$welcomeUI = renderUI({
    req(credentials()$user_auth)
    renderText(paste("Welcome", credentials()$info$name))
  })

  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      showTab("page", "main")
      showTab("page","newstoragelocations")
    } else {
      hideTab("page", "main")
      hideTab("page","newstoragelocations")
    }
  })

  observeEvent(credentials(), {
    req(credentials()$user_auth)
    updateTextInput(
      session = session,
      inputId = "person",
      value = credentials()$info$name
    )
  })

  output$buildingUI = renderUI({
    selectInput("building",
                "building",
                choices = rvals$storageLocations$building %>% unique %>% sort)
  })

  observeEvent(input$add, {
    req(input$type)
    requiredCols = c("purpose" = NA_character_,
                     "person" = NA_character_,
                     "date" = NA_character_)
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
      date = coalesce(input$date, NA)
    ) %>%
      mutate_at(vars(-date), str_trim) %>%
      mutate_at(vars(-date), na_if, "") %>%
      mutate_at(vars(-date), as.character) %>%
      remove_empty("cols") %>%
      tibble::add_column(!!!requiredCols[!names(requiredCols) %in% names(.)])
    if ('barcode' %in% names(inputdf) ||
        'idno' %in% names(inputdf)) {
      inputdf %<>%
        dplyr::select(-any_of('building'))
    }
    print(inputdf)
    print(dput(inputdf))
    inputdf %<>%
      inner_join(rvals$storageLocations)


    rvals$df = bind_rows(inputdf, rvals$df) %>%
      distinct_all() %>%
      select(
        boxno,
        barcode,
        location_id,
        idno,
        type,
        purpose,
        person,
        date,
        building,
        room,
        row,
        unit,
        shelf
      )
  })

  observeEvent(input$submit, {
    myNotification("submitting batch")
    tmpdir = tempdir()
    rio::export(rvals$df, file.path(tmpdir, "importBoxMoves.xlsx"))
    ssh::scp_upload(
      session = sshSession,
      files = file.path(tmpdir, "importBoxMoves.xlsx"),
      to = file.path(dir, "importBoxMoves.xlsx")
    )
    rvals$df = tibble::tibble()
    ssh_exec_wait(sshSession, command = paste0(dir, '/boxtransferauto.sh'))
    ssh_exec_wait(
      sshSession,
      command = paste0(
        'mv ',
        dir,
        '/importBoxMoves.xlsx',
        ' ',
        dir,
        '/archives/importBoxMoves-',
        strftime(Sys.time(), format = "%Y-%m-%d-%H-%M-%OS3"),
        '.xlsx'
      )
    )
    file.remove(file.path(tmpdir, "importBoxMoves.xlsx"))
    myNotification("completed")
  })

  output$table = renderDT({
    DT::datatable(rvals$df, rownames = F)
  })

  observeEvent(input$deleteRow, {
    indx = input$table_rows_selected

    if (length(indx) > 0) {
      rvals$df <- tryCatch(
        rvals$df %>%
          slice(-indx),
        error = function(e) {
          warning(e)
          myNotification(e, type = 'warning')
          return(rvals$df)
        }
      )
    }
  })

  output$storageTable = DT::renderDT({
    DT::datatable(rvals$storageLocations,
                  rownames = F,
                  filter = "top")
  })

  observeEvent(input$newLocationsFile, {
    print(input$newLocationsFile)
    if (!is.null(input$newLocationsFile$datapath))
      rvals$newLocations <-
        tibble::as_tibble(read_xlsx(input$newLocationsFile$datapath, sheet = 1)) %>%
        dplyr::distinct_all()
  })

  observeEvent(input$newLocationsGo, {
    myNotification("uploading new storage locations")
    if (!inherits(rvals$newLocations, "data.frame")) {
      myNotification("must import the .xlsx file first!", type = 'error')
    } else if (!"building" %in% names(rvals$newLocations)) {
      myNotification("must include a column named building")
    } else if (!"room" %in% names(rvals$newLocations)) {
      myNotification("must include a column named room")
    } else if (!"row" %in% names(rvals$newLocations)) {
      myNotification("must include a column named row")
    } else if (!"unit" %in% names(rvals$newLocations)) {
      myNotification("must include a column named unit")
    } else if (!"shelf" %in% names(rvals$newLocations)) {
      myNotification("must include a column named shelf")
    } else {
      upload =
        tryCatch(
          rvals$newLocations %>%
            storage2idno() %>%
            select(idno) %>%
            distinct_all(),
          error = function(e) {
            myNotification(e, type = 'error')
          }
        )
      print("checking if already exists")
      nrb = nrow(upload)
      upload = upload %>%
        filter(!idno %in% rvals$storageLocations$idno)
      nre = nrb - nrow(upload)
      if (nre > 0)
        myNotification(paste(
          "There were",
          nre,
          "already existing locations. These will not be uploaded"
        ))
      print("getting parents")
      if (nrow(upload) > 0) {
        upload = tryCatch(
          upload %>%
            rowwise() %>%
            mutate(parent = getParent(idno)) %>%
            ungroup(),
          error = function(e) {
            myNotification(e)
            return(tibble(idno = NULL, parent = NULL))
          }
        )
      }
      print("checking if parents exist")
      if (nrow(upload) > 0) {
        parents1 = tryCatch(
          upload %>%
            distinct(parent) %>%
            filter(!parent %in% rvals$storageLocations$idno) %>%
            rowwise() %>%
            rename(idno = parent) %>%
            mutate(parent = getParent(idno)) %>%
            ungroup(),
          error = function(e) {
            myNotification(e)
            return(tibble(parent = NULL))
          }
        )
      } else {
        parents1 = tibble(idno = NULL, parent = NULL)
      }
      parents2 = tryCatch(
        parents1 %>%
          distinct(parent) %>%
          filter(!parent %in% rvals$storageLocations$idno),
        error = function(e) {
          myNotification(e)
          return(tibble(idno = NULL, parent = NULL))
        }
      )
      if (nrow(parents2) > 0) {
      parents2 = tryCatch(
        parents2 %>%
          rowwise() %>%
          rename(idno = parent) %>%
          mutate(parent = getParent(idno)) %>%
          ungroup(),
        error = function(e) {
          myNotification(e)
          return(tibble(idno = NULL, parent = NULL))
        }
      )
      }
      parents3 = tryCatch(
        parents2 %>%
          distinct(parent) %>%
          filter(!parent %in% rvals$storageLocations$idno),
        error = function(e) {
          myNotification(e)
          return(tibble(idno = NULL, parent = NULL))
        }
      )
      if (nrow(parents3) > 0) {
      parents3 = tryCatch(parents3 %>%
          rowwise() %>%
          rename(idno = parent) %>%
          mutate(parent = getParent(idno)) %>%
          ungroup(),
        error = function(e) {
          myNotification(e)
          return(tibble(idno = NULL, parent = NULL))
        }
      )
      }
      upload = bind_rows(parents1,parents2,parents3,upload)
      print("adding barcodes")
      lastbarcode =
            tryCatch(suppressWarnings(max(dplyr::tbl(con, "ca_attribute_values") %>%
              dplyr::select(value_longtext1, element_id) %>%
              filter(element_id == 629) %>%
              collect() %>%
              pull(value_longtext1) %>%
        as.integer() %>%
          .[which(!is.na(.))])),error = function(e){
          myNotification(e)
          return(NULL)
          })
      lastbarcode = (lastbarcode + 1):(lastbarcode + nrow(upload))
    upload = tryCatch(upload %>%
      mutate(barcode = lastbarcode),error = function(e){
        myNotification(e)
        return(NULL)
      })

    tmpdir = tempdir()
    rio::export(upload, file.path(tmpdir, "upload.xlsx"))
    ssh::scp_upload(
      session = sshSession,
      files = file.path(tmpdir, "upload.xlsx"),
      to = file.path(dir, "upload.xlsx")
    )
    rvals$df = tibble::tibble()
    ssh_exec_wait(sshSession, command = paste0(dir, '/newStorageLocs.sh'))
    ssh_exec_wait(
      sshSession,
      command = paste0(
        'mv ',
        dir,
        '/upload.xlsx',
        ' ',
        dir,
        '/archives/upload-',
        strftime(Sys.time(), format = "%Y-%m-%d-%H-%M-%OS3"),
        '.xlsx'
      )
    )
    file.remove(file.path(tmpdir, "upload.xlsx"))
    rvals$storageLocations = dbGetQuery(
      con,
      "select ca_storage_locations.location_id, ca_storage_locations.idno, value_longtext1 as barcode from ca_storage_locations join ca_attributes on ca_attributes.row_id = ca_storage_locations.location_id join ca_attribute_values on ca_attribute_values.attribute_id = ca_attributes.attribute_id join ca_metadata_elements on ca_metadata_elements.element_id = ca_attributes.element_id where element_code = 'barcode' and ca_storage_locations.deleted = 0"
    ) %>% idno2storage()
    myNotification("completed")
    }
  })

  output$storageUploadDT = renderDT(rvals$newLocations)
}

shinyApp(ui = ui, server = server)
