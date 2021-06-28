## RNA_seq Raw Data
library(shiny)
library(shinydashboard)
library(knitr)
library(tidyverse)

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
    menuItem("Données brutes", tabName = "raw", icon = icon("area-chart")),
    menuItem("Données groupées", tabName = "gathered", icon = icon("bar-chart")),
    menuItem("Code source", tabName = "source", icon = icon("code"))
    # tags$img(class = "Image", src ="https://p4.wallpaperbetter.com/wallpaper/670/178/355/dna-spiral-genetics-twisted-wallpaper-preview.jpg")
  )),

  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
      fluidRow(
        box(status = "success",width = 6, solidHeader = TRUE, fileInput(inputId = "file", label = "Charger vos données ici",multiple = TRUE, accept = c(".txt","text/csv",
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
                     choices = c(Head = "head", All = "all"),
                     selected = "head")
        
        ),
      
          
    )),
       
     tabItem(tabName = "raw",
      fluidRow(
        box(width = '100%', plotOutput("graphe"), title = "Visualisation des données", status = "success", tableOutput("rawData"))
          )
      ),
    
    tabItem(tabName = "gathered",
            fluidRow(
              box(title = "Variables supplémentaires", status = "success", solidHeader = TRUE,
                  checkboxInput("supp.var1","Temps"),
                  checkboxInput("supp.var2","Virus")
              )
      ),
              box(width = '100%', plotOutput("ggraphe"), title = "Données groupées par temps et/ou par maladie", status = "success")
            
      ),
    
    tabItem(tabName = "source",
            fluidRow(
              box( status = "success", title = "Récupérer les résultats", width =6, solidHeader = TRUE, downloadButton("rapport","Rapport"), align="center"),
              box( status = "success", title = "Récupérer le code", width = 6, solidHeader = TRUE, uiOutput("code"), align="center")
            )
      )
    ))
)
  
    

server <- shinyServer(function(input, output,session) {
  tab <- reactive({ 
    req(input$file)

     tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) 
        df %>%
          pivot_longer(c(`Dengue24ha`,`Dengue24hb`,`Dengue24hc`,`Dengue6da`,`Dengue6db`,`Dengue6dc`,`MOCKA24ha`,`MOCKA24hb`,`MOCKA24hc`,`MOCKA6da`,`MOCKA6db`,`MOCKA6dc`,`MOCKB24ha`,`MOCKB24hb`,`MOCKB24hc`,`MOCKB6da`,`MOCKB6db`,`MOCKB6dc`,`MOCKC24ha`,`MOCKC24hb`,`MOCKC24hc`,`MOCKC6da`,`MOCKC6db`,`MOCKC6dc`,`RVF24ha`,`RVF24hb`,`RVF24hc`,`RVF6da`,`RVF6db`,`RVF6dc`), names_to = "Samplename", values_to = "count")

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
    Tot <- data.frame(colSums(tab()[-1]))
    tab() %>%
      ggplot(data =Tot, mapping = aes(x = colnames(tab()[-1]), y = colSums(tab()[-1])), fill = ) +
      geom_bar(position="dodge", stat="identity", fill="steelblue") +
      ggtitle("Nombre de gène portés par chaque échantillon") + 
      xlab("Echantillon") + 
      ylab("Gènes portés") +
      theme(axis.text.x = element_text(face="bold", color="#543333",size=9, angle=90))
    })
  
  ## Forme de la donnée ? df %>% pivot_longer
  sv <- reactive({ 
    req(input$supp.var1, input$supp.var2)
    
  if(input$supp.var1 == "Temps" & input$supp.var2 == "Virus") {
    output$ggraphe <- renderPlot({
      ggplot(data =Tot, mapping = aes(x = colnames(tab()[-1]), y = colSums(tab()[-1])), fill = temps, color = virus) 
        
    })
    }
  else if (input$supp.var1 == "Temps") {
    output$ggraphe <- renderPlot({
      
      ggplot(data =Tot, mapping = aes(x = colnames(tab()[-1]), y = colSums(tab()[-1])), fill = temps ) 
        
    })
    }
 else if (input$supp.var2 == "Virus") {
   ggplot(data =Tot, mapping = aes(x = colnames(tab()[-1]), y = colSums(tab()[-1])), fill =virus ) 
     
   output$ggraphe <- renderPlot({
     
   })
 }
    
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