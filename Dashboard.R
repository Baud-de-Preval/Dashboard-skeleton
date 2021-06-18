library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Dermatose nodulaire bovine"),
  dashboardSidebar(sidebarMenu(title = "Menu"),
               menuItem("Entrée des données", tabName = "données", icon = icon("dashboard")),
               menuItem("Vue d'ensemble", tabName = "ensemble", icon = icon("dashboard")),
               menuItem("Analyse", tabName = "analyse", icon = icon("dashboard")),
               menuItem("Rapport", tabName = "rapport", icon = icon("dashboard"))))

  dashboardBody(
    fluidRow(tabItems(
      tabItem(tabName = "données",
              box(fileInput(inputId = "data", label = "Charger vos données ici"))),
      tabItem(tabName = "Vue d'ensemble",
              textOutput(outputId = "summary"), plotOutput(outputId = "plot"))),
      tabItem(tabName = "analyse",
              tableOutput(outputId = "Graphe")),
      tabItem(tabName = "rapport",
              radioButtons('format', 'Format du document', c('PDF', 'HTML', 'Word'), inline = TRUE),
              downloadButton(outputId = "download", label = "Télécharger"))))

server <- function(input, output) {
  output$summary <- renderTable({
    summary(input$data)})
  output$plot <- renderPlot({
    hist(imput$data)})
  #data <- #Le rapport
  output$download <- downloadHandler(
    filename = function() {
      paste("Rapport", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write(data, file)
    }
  )
}
shinyApp(ui = ui, server = server)
