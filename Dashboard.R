## RNA_seq Raw Data
library(shiny)
library(shinydashboard)
library(ggplot2)
library(knitr)
library(magrittr)
library(dplyr)

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
    menuItem("Données brutes", tabName = "raw", icon = icon("bar-chart")),
    menuItem("Code source", tabName = "source", icon = icon("code"))
    # tags$img(class = "Image", src ="https://p4.wallpaperbetter.com/wallpaper/670/178/355/dna-spiral-genetics-twisted-wallpaper-preview.jpg")
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
        box(plotOutput("graphe",click = "plot_click"), title = "Visualisation des données", status = "success", tableOutput("rawData"))
          )
      ),
    
    tabItem(tabName = "source",
      fluidRow(
        box( status = "success", title = "Récupérer les résultats", width =6, solidHeader = TRUE, downloadButton("rapport","Rapport"), align="center"),
        box( status = "success", title = "Récupérer le code", width = 6, solidHeader = TRUE, uiOutput("code"), align="center")
      )
            ))
  ))
    

server <- shinyServer(function(input, output,session) {
  tab <- reactive({ 
    req(input$file)

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
  
  output$contents <- renderTable({
    rep(head(tab()))
    })

  output$graphe <- renderPlot({
    vars <- names(tab())
    tab() %>% ggplot(tab(), mapping = aes(y = colSums(tab()[,1]), x =tab()[[2,]])) + geom_bar(position="dodge", stat="identity", fill="steelblue")
       
    })
  
  url <- a("ici", href="https://github.com/Baud-de-Preval/Dashboard-skeleton/blob/master/Dashboard.R")
  output$code <- renderUI({
    tagList("Le lien vers le code :", url)
  })
  
  output$rapport <- downloadHandler(
    filename = "Rapport.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Rapport.rmd")
      file.copy("Rapport.rmd", tempReport, overwrite = TRUE)
      # A compléter
      params <- list(n = input$slider)
    
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})

shinyApp(ui, server)