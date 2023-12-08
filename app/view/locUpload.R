# app/view/locUpload

box::use(
  shiny[tabPanel, h3, p, fileInput, actionButton,NS,moduleServer,observeEvent,req],
  DT[DTOutput,renderDT],
  tibble[as_tibble,tibble],
  dplyr[`%>%`,distinct_all,select,filter,mutate,rowwise,ungroup,distinct,rename,bind_rows,tbl,collect,pull],
  rio[export],
  ssh[ssh_exec_wait,scp_upload],
  DBI[dbGetQuery],
  readxl[read_xlsx],
)

box::use(app / logic / myNotification[myNotification],
         app/logic/storage2idno[storage2idno],
         app/logic/getParent[getParent],
         app/logic/getStorageLocs[getStorageLocs],
         app/logic/getType[getType],
)

#' @export
ui = function(id){
  ns = NS(id)
  tabPanel(
    title = "Upload New Storage Locations",
    value = 'newstoragelocations',
    h3('instructions'),
    p(
      "Upload an excel file with columns having these exact names: building, room, row, unit, shelf. Fill out the information for each storage location as appropriate. Remember to spell the names exactly as spelled in the database: e.g., CSB not Community Services Building, Alameda not Ala,  B not b, etc. Next, upload the spreadsheet using the import button and press go. Wait for the confirmation notification to confirm it worked and then check the Storage Locations tab."
    ),
    fileInput(ns("newLocationsFile"), "import", accept = c(".xlsx")),
    DTOutput(ns('storageUploadDT')),
    actionButton(ns("submit"), "submit")
  )
}

#' @export
server = function(id,sshSession, rvals, con,dir){
  moduleServer(id, function(input, output, session){

    observeEvent(input$newLocationsFile, {
      print(input$newLocationsFile)
      if (!is.null(input$newLocationsFile$datapath))
        rvals$newLocations <-
          as_tibble(read_xlsx(input$newLocationsFile$datapath, sheet = 1)) %>%
          distinct_all()
    })

    observeEvent(input$submit, {
      req(nrow(rvals$newLocations) > 0)
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
        message("checking if already exists")
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
        message("getting parents")
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
        message("checking if parents exist")
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
        message("adding barcodes")
        lastbarcode =
          tryCatch(suppressWarnings(max(tbl(con, "ca_attribute_values") %>%
                                          select(value_longtext1, element_id) %>%
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
        message("adding types")
        upload = upload %>% getType()
        message("uploading")
        tmpdir = tempdir()
        cols = c("idno", "parent", "barcode", "type")
        upload = upload %>% select(any_of(cols)) %>%
          filter(!is.na(idno)) %>%
          filter(idno != "")
        req()
        export(upload, file.path(tmpdir, "upload.xlsx"))
        scp_upload(
          session = sshSession,
          files = file.path(tmpdir, "upload.xlsx"),
          to = file.path(dir, "upload.xlsx")
        )
        rvals$df = tibble()
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
        rvals$storageLocations = getStorageLocs(con)
        myNotification("completed")
      }
    })

    output$storageUploadDT = renderDT(rvals$newLocations)
  })
}
