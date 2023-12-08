box::use(
  DBI[dbConnect,dbGetQuery],
  RMySQL[MySQL],
  ssh[ssh_connect],
  shiny[navbarPage,reactiveValues,NS,moduleServer,observeEvent,showTab,hideTab],
  shinyjs[useShinyjs],
  tibble[tibble],
  dplyr[tbl,collect,`%>%`]
)

box::use(
  app/view/login,
  app/view/manual,
  app/view/batch,
  app/view/locations,
  app/view/locUpload,
)

box::use(
  app/logic/myNotification[myNotification],
  app/logic/getStorageLocs[getStorageLocs],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = "CASR Box Updater",
    id = ns("page"),
    useShinyjs(),
    login$ui(ns('login')),
    manual$ui(ns('manual')),
    batch$ui(ns('batch')),
    locations$ui(ns('locations')),
    locUpload$ui(ns('locUpload')),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    con = dbConnect(
      MySQL(),
      host = "10.126.24.122",
      dbname = "ca",
      user = Sys.getenv("uid"),
      password = Sys.getenv("pwd"),
      port = 3306
    )

    con2 = dbConnect(
      MySQL(),
      host = "10.126.24.122",
      dbname = "shiny",
      user = Sys.getenv("uid"),
      password = Sys.getenv("pwd"),
      port = 3306
    )

    dir = "/mnt/storage/public"

    sshSession = ssh_connect("ca@10.126.24.122", passwd = Sys.getenv("pwd"))

    database = tbl(con2,"user_data") %>%
      collect()

    rvals = reactiveValues(
      df = tibble(),
      storageLocations = getStorageLocs(con)
    )

    credentials = login$server('login',id,database,con2)
    manual$server('manual',id,sshSession,credentials,rvals,dir)
    batch$server('batch',sshSession,rvals,dir)
    locations$server("locations",rvals)
    locUpload$server("locUpload", sshSession,rvals, con,dir)


    observeEvent(credentials()$user_auth, {
      if (credentials()$user_auth) {
        showTab("page", "manualentry")
        showTab("page","newstoragelocations")
        showTab("page","batchimport")
      } else {
        hideTab("page", "manualentry")
        hideTab("page","newstoragelocations")
        hideTab("page","batchimport")
      }
    })
  })
}
