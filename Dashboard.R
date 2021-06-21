## RNA_seq Raw Data
library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(skin = "red", dashboardHeader(title = "Visualisation des données brutes", titleWidth = 400),
  dashboardSidebar(
    menuItem("Importation des données", tabName = "data", icon = icon("glyphicon glyphicon-floppy-open")),
    menuItem("Données brutes", tabName = "raw", icon = icon("glyphicon glyphicon-stats"))
  ))
  dashboardBody(
    fluidRow(
      box(status = "success",width = 6, solidHeader = TRUE, fileInput(inputId = "RNA-seq", label = "Charger vos données ici", accept = ".txt")),
      box(plotOutput(inputId = "Plot1", height = 300))
  )
)

server <- function(input, output) {
  output$Plot1 <- renderPlot({
    file <-input$RNA-seq
  })
}

shinyApp(ui, server)