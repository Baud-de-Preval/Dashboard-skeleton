## RNA_seq Raw Data
library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  skin = "green", 
  dashboardHeader(title = "Visualisation des données brutes", titleWidth = 300),
  dashboardSidebar(
    menuItem("Importation des données", tabName = "data", icon = icon("file")),
    menuItem("Données brutes", tabName = "raw", icon = icon("bar-chart"))
  ),

  dashboardBody(
    tabItem("data",
    fluidRow(
      box(status = "success",width = 6, solidHeader = TRUE, fileInput(inputId = "file", label = "Charger vos données ici", accept = c(".txt","text/csv",
                                                                                                                                                  "text/comma-separated-values,text/plain",
      tags$hr(),tags$hr(),                                                                                                                                            ".csv",".ods"))),
      box(checkboxInput("header", "Header", TRUE)),
  
    
        # Input: Select separator ----
      box(radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
 
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
      ),
      mainPanel(
        tableOutput("contents")
        
      )
  ))),
      tabItem("raw",
      fluidRow(box(tableOutput("data")
          ))
          ))

server <- function(input, output) {

  output$contents <- renderTable({
    
    rep(head(input$file))
    
    
    tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safe Error if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })

      output$plot1 <- renderPlot({
      ggplot(df, aes(x = df[1,1], y= df[0,1])) + theme(axis.text.x = element_text(face="bold", color="#997643",size=9, angle=32))
    })

}

shinyApp(ui, server)