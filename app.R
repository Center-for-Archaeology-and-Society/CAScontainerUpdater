library(shiny)
library(dplyr)
library(DT)
library(shinyjs)

ui <- navbarPage(
  title = "CAS Box Updater",

  useShinyjs(),

  tabPanel("login"),
  tabPanel("main",
           wellPanel(
             fluidRow(
               column(width = 1,textInput("id","id")),
               column(width = 2,textInput("idno","idno")),
               column(width = 1,textInput("building","building")),
               column(width = 1,textInput("room","room")),
               column(width = 1,textInput("row","row")),
               column(width = 1,textInput("unit","unit")),
               column(width = 1,textInput("shelf","shelf")),
               column(width = 2,selectInput("type","type of move", choices = c("permanent","temporary"))),
               column(width = 2,textInput("purpose","purpose of transfer"))
             ),
             fluidRow(
               column(width = 3,shinyjs::disabled(textInput("person","person"))),
               column(width = 3,dateInput("date","date")),
             ),
             fluidRow(
               column(width = 3, actionButton(inputId = "add","add"))
             )
           ),
           DT::DTOutput(outputId = "table"),
           actionButton(inputId = "submit",label = "submit batch")
  )
)
server <- function(input, output) {

  rvals = reactiveValues(df = tibble())

  observeEvent(input$add,{
    rvals$df = tibble(
      id = input$id,
      idno = input$idno,
      building = input$building,
      room = input$room,
      row = input$row,
      unit = input$unit,
      shelf = input$shelf,
      type = input$type,
      purpose = input$purpose,
      person = input$person,
      date = input$date
    )
  })

  observeEvent(input$submit, {
    showNotification("tough luck, but this doesn't do anything")
  })

  output$table = renderDT({
    rvals$df
  })
}

shinyApp(ui = ui, server = server)
