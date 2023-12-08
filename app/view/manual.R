# app/login/manual

box::use(shiny[tabPanel, wellPanel, fluidRow, column, textInput, uiOutput, selectInput, dateInput, actionButton, observeEvent, NS, moduleServer, renderUI, req, updateTextInput, selectizeInput, HTML, h4],
         DT[DTOutput, renderDT, datatable],
         ssh[ssh_exec_wait],
         dplyr[mutate_at, inner_join, select, bind_rows, distinct_all, vars, coalesce, na_if, slice],
         tibble[tibble, add_column],
         stringr[str_trim],
         magrittr[`%<>%`,`%>%`],
         janitor[remove_empty],
         tidyselect[any_of])

box::use(app / logic / myNotification[myNotification],
         app / logic / submitBatch[submitBatch],)

#' @export
ui = function(id) {
  ns = NS(id)
  tabPanel(
    title = "Manual Entry",
    value = "manualentry",
    wellPanel(
      fluidRow(column(width = 3, textInput(
        ns("boxno"), "boxno"
      ))),
      fluidRow(
        column(width = 2, textInput(ns("barcode"), "barcode")),
        column(width = 2, textInput(ns("idno"), "idno")),
        column(width = 2, uiOutput(ns('buildingUI'))),
        column(width = 2, textInput(ns("room"), "room")),
        column(width = 1, textInput(ns("row"), "row")),
        column(width = 1, textInput(ns("unit"), "unit")),
        column(width = 2, textInput(ns("shelf"), "shelf"))
      ),
      fluidRow(
        column(width = 3, textInput(ns("person"), "Person(s) responsible")),
        column(width = 3, selectInput(
          ns("relation"),
          "type of move",
          choices = c("permanent", "temporary", "returned")
        )),
        column(width = 3, textInput(ns("purpose"), "purpose of transfer")),
        column(width = 3, dateInput(ns("date"), "date")),
      ),
      fluidRow(column(
        width = 3, actionButton(inputId = ns("add"), "add")
      ))
    ),
    fluidRow(column(width = 3, wellPanel(
      actionButton(ns("deleteRow"), "Delete selection")
    ))),
    DTOutput(outputId = ns("table")),
    actionButton(inputId = ns("submit"), label = "submit"),
    fluidRow(column(7, wellPanel(
      h4("Log"),
      uiOutput(ns("log"))
    )))

  )
}

#' @export
server = function(id,
                  id2,
                  sshSession,
                  credentials,
                  rvals,
                  dir) {
  moduleServer(id, function(input, output, session) {
    ns = NS(paste0(id2, "-", id))
    # make sure directory is readable
    observeEvent(input$add, {
      ssh_exec_wait(sshSession, command = 'sudo chmod -R +777 /mnt/storage/public')
    })

    observeEvent(credentials(), {
      req(credentials()$user_auth)
      message("updating person responsible")
      updateTextInput(
        session = session,
        inputId = "person",
        value = credentials()$info$name
      )
    })

    output$buildingUI = renderUI({
      selectizeInput(
        ns("building"),
        "building",
        choices = rvals$storageLocations$building %>% unique %>% sort,
        multiple = T,
        options = list(maxItems = 1)
      )
    })

    observeEvent(input$add, {
      req(input$relation)
      myNotification("adding box")
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
        relation = input$relation,
        purpose = input$purpose,
        person = input$person,
        date = coalesce(input$date, NA)
      ) %>%
        mutate_at(vars(-date), str_trim) %>%
        mutate_at(vars(-date), na_if, "") %>%
        mutate_at(vars(-date), as.character) %>%
        remove_empty("cols") %>%
        add_column(!!!requiredCols[!names(requiredCols) %in% names(.)])
      if ('barcode' %in% names(inputdf) ||
          'idno' %in% names(inputdf)) {
        inputdf %<>%
          select(-any_of('building'))
      }
      # print(inputdf)
      # print(dput(inputdf))
      inputdf = tryCatch(
        inputdf %>%
          inner_join(rvals$storageLocations),
        error = function(e) {
          myNotification("error in join")
          return(NULL)
        }
      )


      rvals$df = bind_rows(inputdf, rvals$df) %>%
        distinct_all() %>%
        select(any_of(
          c(
            "boxno",
            "barcode",
            "location_id",
            "type",
            "idno",
            "relation",
            "purpose",
            "person",
            "date",
            "building",
            "room",
            "row",
            "unit",
            "shelf"
          )
        ))
    })


    observeEvent(input$submit, {
      req(nrow(rvals$df) > 0)
      rvals$log = submitBatch(rvals, sshSession, dir)
    })

    output$log = renderUI({
      HTML(rvals$log)
    })

    output$table = renderDT({
      datatable(rvals$df, rownames = F)
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

  })
}
