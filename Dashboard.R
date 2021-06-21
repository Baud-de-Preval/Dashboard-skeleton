library(shiny)
library(shinydashboard)
library(knitr)
### Erreur de parenthèses

ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Dermatose nodulaire bovine", titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(title = "Menu",
        menuItem("Importation des données", tabName = "données", icon = icon("import")),
        menuItem("Vue d'ensemble", tabName = "ensemble", icon = icon("th")),
        menuItem("Analyse", tabName = "analyse", icon = icon("stats")),
        menuItem("Rapport", tabName = "rapport", badgeLabel = "Nouveau", badgeColor = "red", icon = icon("paperclip")),
        menuItem("Code source", href = "https://github.com/Baud-de-Preval/", icon = icon("file-code-o"))
    )
  )
)
###  
   dashboardBody(
    tabItems(
      tabItem(tabName = "données",
        fluidRow(column(3 ,align ="center"),
          box(status = "success",width = 6, solidHeader = TRUE, fileInput(inputId = "data", label = "Charger vos données ici")))),
      
      tabItem(tabName = "Vue d'ensemble",
        fluidRow(align ="center"),
          tabBox(title = "Exploration des données", id = "explore",
            tabPanel("Graphic representation"),
            tabPanel("Summary"))),
            tableOutput(outputId = "Graphe"),
          
      tabItem(tabName = "analyse",
        fluidRow(align = "center"),
        box(status = "info",width = 6, solidHeader = TRUE, tableOutput(outputId = "summary"), plotOutput(outputId = "plot", inline = TRUE))),
              
      tabItem(tabName = "rapport",
          textOutput(outputId = "report"),
          radioButtons('format', 'Format du document', c('PDF', 'HTML', 'Word'), inline = TRUE),
          downloadButton(outputId = "download", label = "Télécharger")),
     
       shinyUI(
        fluidPage(
          uiOutput('markdown')
        )
    )))

server <- function(input, output) {
  output$summary <- renderTable({
    summary(input$data)}) 
  output$plot <- renderPlot({
    hist(input$data)})
  #data <- #Le rapport
  output$report <- renderUI({
    HTML(markdown::markdownToHTML(knit('RMarkdownFile.rmd')))
    })

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


