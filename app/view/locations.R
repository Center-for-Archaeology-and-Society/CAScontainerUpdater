# app/view/locations

box::use(
  shiny[tabPanel,moduleServer,NS,req,h2],
  DT[DTOutput,renderDT,datatable],
)

#' @export
ui = function(id){
  ns = NS(id)
  tabPanel(
    title = "Storage Locations",
    value = "storageLocations",
    h2("Current Storage Locations"),
    DTOutput(outputId = ns("storageTable"))
  )
}

#' @export
server = function(id,rvals){
  moduleServer(id, function(input, output, session){
    output$storageTable = renderDT({
      req(rvals$storageLocations)
      datatable(rvals$storageLocations,
                    rownames = F,
                    filter = "top")
    })
  })
}

