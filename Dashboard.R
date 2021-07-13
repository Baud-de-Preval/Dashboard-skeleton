library(shiny)
library(shinydashboard)
library(knitr)
library(tidyverse)
library(readxl)
library(reshape2)
library(plyr)
library(janitor)

# Initialisation



ui <- dashboardPage(
  skin = "green", 
  dashboardHeader(title = "Lumpy skin disease data analysis", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem("Data uploader", tabName = "data", icon = icon("file")),
    menuItem("Raw data", tabName = "raw", icon = icon("area-chart")),
    menuItem("Grouped data", tabName = "gathered", icon = icon("bar-chart"), sliderInput("MPS", "MPS lower threshold :", 0,150,60),
             menuSubItem("By sample", tabName = "sample", icon = icon("list-alt"))
),
    menuItem("Source code", tabName = "source", icon = icon("code"))
    # tags$img(class = "Image", src ="https://p4.wallpaperbetter.com/wallpaper/670/178/355/dna-spiral-genetics-twisted-wallpaper-preview.jpg")
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(status = "success",width = 6, solidHeader = TRUE, fileInput(inputId = "file", label = "Please upload your data in xlsx or csv format", multiple = TRUE, accept = c("csv","xlsx"))),
                box(tableOutput("contents"),width = 6, title = "Data overview", status = "success", soliderHeader = TRUE, collapsible = T, collapsed = T),
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
                    
                )
                
                )
              ),
      
      tabItem("raw",
        fluidRow(
          box(tableOutput("Summary"), title = "Summary", status = "success", solidHeader = TRUE)
        )
          
        ),
      
      tabItem("sample",
              fluidRow(
                box(plotoutput = "datahard", status = "success", title = "Overview on raw data", solidHeader = TRUE),
                box(title = "Samples display",
                     (checkboxInput("Q27824","Q27824",FALSE)),
                     (checkboxInput("Q27825","Q27825",FALSE)),
                     (checkboxInput("Q27826","Q27826",FALSE)),
                     (checkboxInput("Q27827","Q27827",FALSE)),
                     (checkboxInput("Q27828","Q27828",FALSE)),
                     (checkboxInput("Q27829","Q27829",FALSE)),
                     (checkboxInput("Q27830","Q27830",FALSE)),
                     (checkboxInput("Q27831","Q27831",FALSE)),
                     (checkboxInput("Q27832","Q27832",FALSE)),
                     (checkboxInput("all","All",TRUE))
              )
            )),
      
      tabItem(tabName = "source",
              fluidRow(
                box( status = "success", title = "Get the report", width =6, solidHeader = TRUE, downloadButton("rapport","Rapport"), align="center"),
                box( status = "success", title = "Get the code", width = 6, solidHeader = TRUE, uiOutput("code"), align="center")
              )
            )
          
      
        
      )))
    
  

server <- shinyServer(function(input, output,session) {
  tab <- reactive({ 
    req(input$file)
    
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
    output$contents <- renderTable({
      rep(head(df))
    })
    
    # Traitement du tableau d'entrée
    df %>% separate(Origine_Query,into=c("sample","query"),sep=6) -> df
    df %>% group_by(sample,Accession,sequence) %>% unique -> df
    Sample <- unique(df$sample)
    N = as.numeric(table(df$sample))
    
    
 
    output$Summary <- renderPlot({
      df %>% ggplot(mapping = aes(y= N, x= Sample,color = colSums(tab()$'Mascot Peptide Score'))) +
        geom_bar(fill = "steelblue", stat = "identity") +
        labs(color="Mascot score") +
        scale_color_gradient(low="blue", high="red") +
        geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
        theme(axis.text.x = element_text(face="bold", color="black",angle = 32)) +
        ggtitle("Number of protein in each sample") + 
        xlab("Sample") + 
        ylab("Number of protein") 
    })
    
    output$sample <- renderplot({    ## Est ce que le nb d'échantillon sera tjrs constant ?
      Essay <- c("Q27824", "Q27825", "Q27826", "Q27827", "Q27828", "Q27829", "Q27830", "Q27831", "Q27832")
      if ("all" == T){
        Nb = 9
      }
      else { 
        Nb =table(Essay[TRUE])    # Déterminer le nombre d'échantillon selectionné
      }
      Nb_pep<-matrix(rep(0,ncol*nrow), ncol=Nb, nrow=5, byrow=FALSE) # Création d'une matrice vide
      colnames(Nb_pep) = Sample
      name = c("1 peptide", "2 to 5 peptides", "5 to 20 peptides", " 20 to 50 peptides","more than 50 peptides")
      rownames(Nb_pep)<- name
      
      Essay <-Essay[TRUE]
      i=0                         # Remplir une matrice nb peptide/prot/échantillon
      for (l in 1:Nb){
        i=i+1
        df <- subset(df, Query == Essay[TRUE][l])
        nb = as.numeric(table(df$Accession))
        Nb_pep[1,i] <- sum(nb==1)
        Nb_pep[2,i] <- sum(nb>1 & nb<= 5)
        Nb_pep[3,i] <- sum(nb>5 & nb<=20)
        Nb_pep[4,i] <- sum(nb>20 & nb<=50)
        Nb_pep[5,i] <- sum(nb>50)
      }
      Nb_pep %>% pivot_longer(sample, names_to = "Trial", values_to = "Number") -> Nb_pep
      
      df %>% ggplot(mapping = aes(y= unlist(Nb_pep[,2]), x= rep(rownames(Nb_pep),Nb), levels = rep(name, Nb)), fill =Essay[TRUE]) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(face="bold", color="black", size = 6)) +
        geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25, size = 2.5) + 
        ggtitle("Number of peptide composing proteins") + 
        ylab("Protein composition")+ 
        theme(legend.position="none") +
        xlab("")+
        facet_wrap(~ Trial, scales = "free_x") +
        aes(fill = as.factor(Trial)) 
     
    })
})     
    url <- a("ici", href="https://github.com/Baud-de-Preval/Dashboard-skeleton/blob/master/Dashboard.R")
    output$source <- renderUI({
      tagList("Le lien vers le code :", url)
    })
})

shinyApp(ui, server)
