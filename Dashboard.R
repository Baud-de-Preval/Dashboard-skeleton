## RNA_seq Raw Data
library(shiny)
library(shinydashboard)
library(ggplot2)

.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)

ui <- dashboardPage(
  skin = "green", 
  dashboardHeader(title = "Visualisation des données brutes", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem("Importation des données", tabName = "data", icon = icon("file")),
    menuItem("Données brutes", tabName = "raw", icon = icon("bar-chart"))
  )),

  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
      fluidRow(
        box(status = "success",width = 6, solidHeader = TRUE, fileInput(inputId = "file", label = "Charger vos données ici", accept = c(".txt","text/csv",
                "text/comma-separated-values,text/plain",".csv",".ods"))),
        box(tableOutput("contents"),width = 6, title = "Aperçu des données", status = "success", soliderHeader = TRUE, collapsible = T, collapsed = T),
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
                     choices = c(Head = "head"),
                     selected = "head")
        
        ),
      
          
    )),
       
     tabItem(tabName = "raw",
      fluidRow(
        box(plotOutput("graphe",click = "plot_click"),verbatimTextOutput("info"), title = "Visualisation des données", status = "success")
          )
      )
  ))
    )

server <- shinyServer(function(input, output,session) {
  tab <- reactive({ 
    req(input$file)
    inFile <- input$file
    
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
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$contents <- renderTable({
    
    rep(head(tab()))
    
    })

  output$graphe <- renderPlot({
    pd <- input$file
    pd1 <- ggplot(pd, mapping = aes(y = colnames(tab()[1]), x = colnames(tab()[2,])))  
      print(pd1)
    })
})

shinyApp(ui, server)