# app/view/batch

box::use(
  shiny[tabPanel, h3, p, fileInput, actionButton,NS,moduleServer,observeEvent,fluidRow,column,wellPanel,h4,uiOutput,renderUI,HTML,req],
  DT[DTOutput,renderDT,datatable],
  ssh[ssh_exec_wait],
  dplyr[inner_join, select, bind_rows, distinct_all],
  readxl[read_xlsx],
  tibble[as_tibble,add_column],
  tidyselect[any_of],
  magrittr[`%>%`],
)

box::use(app / logic / submitBatch[submitBatch],)

#' @export
ui = function(id){
  ns = NS(id)
  tabPanel(title = 'Batch Import',
           value = 'batchimport',
           h3('instructions'),
           p("Upload an excel or csv file containing any of the following columns: boxno, barcode, idno, building, room, row, unit, shelf, person, type, purpose, date. Fill out the information for each column as appropriate. The only required columns are boxno (remember to include a 'T-' if it is a temporary box number and relation (permanent or temporary). The other columns are not strictly required except as needed to match with the existing storage locations. Remember to spell the names exactly as spelled in the database: e.g., CSB not Community Services Building, Ala not Alameda,  B not b, etc. Next, match the spreadsheet with storage locations using the add button and press submit batch. Wait for the confirmation notification to confirm it worked."),
           fileInput(ns('importBoxes'),'select excel or csv file'),
           actionButton(ns('add'),'add'),
           DTOutput(outputId = ns("table")),
           actionButton(inputId = ns("submit"), label = "submit batch"),
           fluidRow(column(7,wellPanel(
             h4("Log"),
             uiOutput(ns("log"))
           )))
  )
}

#' @export
server = function(id,sshSession,rvals,dir){
  moduleServer(id, function(input, output, session){

    observeEvent(input$importBoxes, {
      print(input$importBoxes)
      if (!is.null(input$importBoxes$datapath))
        rvals$df <-
          as_tibble(read_xlsx(input$importBoxes$datapath, sheet = 1)) %>%
          distinct_all()
    })

    observeEvent(input$add, {
      req(nrow(rvals$df) > 0)
      ssh_exec_wait(sshSession, command = 'sudo chmod -R +777 /mnt/storage/public')
      rvals$df = tryCatch(
        rvals$df %>%
          inner_join(rvals$storageLocations),
        error = function(e) {
          myNotification("error in join")
          return(NULL)
        }
      )

      requiredCols = c("purpose" = NA_character_,
                       "person" = NA_character_,
                       "date" = NA_character_)

      rvals$df = rvals$df %>%
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
        )) %>%
        add_column(!!!requiredCols[!names(requiredCols) %in% names(.)])
    })

    observeEvent(input$submit, {
      req(nrow(rvals$df) > 0)
      req("idno" %in% names(rvals$df))
      req("boxno" %in% names(rvals$df))
      req("type" %in% names(rvals$df))
      req("relation" %in% names(rvals$df))
      rvals$log = submitBatch(rvals, sshSession, dir)
    })

    output$table = renderDT({
      datatable(rvals$df, rownames = F)
    })

    output$log = renderUI({
      HTML(rvals$log)
    })

  })
}
