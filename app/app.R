

ui <- navbarPage(
  title = "CASR Box Updater",
  id = "page",
  useShinyjs(),
  login$ui('login'),
  manual$ui('manual'),
  batch$ui('batch'),
  locations$ui('locations'),
  locUpload$ui('locUpload'),
)

server <- function(input, output, session) {

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

  database = safeImport("static/database.RDs",sshSession)

  rvals = reactiveValues(
    df = tibble(),
    storageLocations = dbGetQuery(
      con,
      "select ca_storage_locations.location_id, ca_storage_locations.idno, value_longtext1 as barcode from ca_storage_locations join ca_attributes on ca_attributes.row_id = ca_storage_locations.location_id join ca_attribute_values on ca_attribute_values.attribute_id = ca_attributes.attribute_id join ca_metadata_elements on ca_metadata_elements.element_id = ca_attributes.element_id where element_code = 'barcode' and ca_storage_locations.deleted = 0"
    ) %>% idno2storage()
  )

  # credentials = login$server('login',database)
  # manual$server('manual',sshSession)
  # batch$server('batch',sshSession)
  locations$server("locations",rvals)
  # locUpload$server("locUpload")


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
}

shinyApp(ui = ui, server = server)
