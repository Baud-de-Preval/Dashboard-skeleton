### Packages

library(shiny)
library(shinydashboard)
library(knitr)
library(tidyverse)
library(readxl)
library(reshape2)
library(ggpubr)
library(janitor)
library(FactoMineR)
library(factoextra)
library(shinyWidgets)
library(plotly)
library(vegan)
library(UpSetR)
require(ggrepel)
library(grid)
library(heatmaply)
library(d3heatmap)
library(viridis)
library(hrbrthemes)
library(MASS)
library(shinydlplot)
library(RVenn)
library(shinyalert)

#####################   Functions   ########################

### Tidy the input file   ###

parse_protein_file = function(filepath){
  # First extract samples name from line 18 to 19
  samples = readxl::read_xlsx(filepath,
                              skip=17,n_max=1,col_names = F)
  # get number of cells 
  length = dim(samples)[[2]]
  # extract columns with sample ID (start at 11, then 3 cells apart)
  samples = samples %>%  dplyr::select(seq(11,length,3))
  # transform line as column ( t == transpose)
  samples = t(samples)[,1]
  # remove useless line names
  names(samples) = NULL
  ##### Now for the real data: Start at line 19:  
  data = readxl::read_xlsx(filepath,
                           skip=19,col_names = T)
  ##### isolate the first 15 columns (fix, generic data on proteins)
  global_data = data %>% dplyr::select(1:15)
  # Replace ambiguous column name "sample" with "BestSpecificSample
  colnames(global_data)[7]="BestSpecificSample"
  # initialize empty data frame
  finaldata = data.frame()
  # Looping from 1 to number of samples
  for(i in 1:length(samples)){
    # positions of the current sample info in data (three columns: SC, LSAT, %LSAT)
    first = 16+(i-1)*3
    last = 16+i*3-1
    data %>% dplyr::select(first:last) -> sample_data
    colnames(sample_data)= c("SC","NSAF","%NSAF")
    # Copy the generic data on protein -> appear for each sample
    tmp_global = global_data
    # Fill the sample column with current sample name 
    tmp_global$sample = samples[i]
    # bind columns of global data with data for samples
    tmp = cbind(tmp_global,sample_data)
    #bind rows from this sample total data with the previous ones
    finaldata = rbind(finaldata,tmp)
  }
  return(finaldata)
}

### Read the supplementary file ###

parse_supp_file = function(file){
  otherdata = readr::read_csv(file, col_names = T)
  return(otherdata)
}

## Make hist ##

make_hist = function(data, value, math){
  data %>%
    dplyr::select(sample, value, Organism) %>%
    as.data.frame() -> histdata2
  
  count(histdata2, Organism, sample) -> N2
  View(N2)
  p <- histdata2 %>%
    ggplot() +
    geom_boxplot(aes(x= Organism, y =get(value), fill = Organism), size = 0.2, width = 0.2) +
    geom_text(data= N ,aes(x= Organism, y = n, label = n), position=position_dodge(width=0.6), vjust=1.5) +
    theme_bw() + facet_wrap(~ sample)
  if (math == FALSE){
    return(p + ylab(paste(value)))
  }
  else {
    return(p + scale_y_log10() + ylab(paste("Log", value, sep = "")))
  }
}

## Make an UpsetR ##

make_upset = function(data, listof_sample, species, order, pace){
  data %>% 
    filter(`#specific peptides`>1) %>% 
    filter(grepl(species,Organism)) %>% 
    dplyr::select(`Protein accession`,sample,SC) %>% 
    mutate(SC = as.integer(ifelse(SC>1,1,0))) %>%
    pivot_wider(id_cols = `Protein accession`,names_from=sample,values_from=SC)  %>%
    as.data.frame -> updata 
  updata[is.na(updata)] <- 0
  return(upset(updata, nsets = length(listof_sample), order.by = order, decreasing = pace, number.angles = 30, point.size = 4, line.size = 1.5, 
        mainbar.y.label = "Protein Intersections", sets.x.label = "Protein per sample", 
        text.scale = c(2, 2, 1, 1, 1.2, 2), main.bar.color = "steelblue"))
}

## Make intersection & union

get_prot_of_sample = function(data, inputsample){
  data %>% 
    dplyr::filter(sample == inputsample) %>% 
    dplyr::filter(SC > 0) %>%
    dplyr::select(`Protein accession`) %>% 
    as.vector -> tmp
  return(tmp$`Protein accession`)
}

make_inter = function(data,list_of_sample, wanted_sample, species){
  obj = list()
  for (i in 1:length(list_of_sample)){
    obj[[length(obj)+1]] <- get_prot_of_sample(data, list_of_sample[i])
    names(obj)[i] = list_of_sample[i]
  } 
  Set1 = obj[names(obj) %in% wanted_sample]
  toinclude = as.data.frame(RVenn::overlap(Venn(Set1)))
  data %>% 
    filter(sample == wanted_sample) %>%
    filter(`Protein accession` == toinclude[,1]) %>%
    filter(Organism == species) -> SET1
  return(SET1[,c(1,2,7,8,9)])
}

make_interbis = function(data,list_of_sample, wanted_sample, species){
  obj = list()
  for (i in 1:length(list_of_sample)){
    obj[[length(obj)+1]] <- get_prot_of_sample(data, list_of_sample[i])
    names(obj)[i] = list_of_sample[i]
  } 
  Set2 = obj[!names(obj) %in% wanted_sample]
  toexclude = as.data.frame(RVenn::unite(Venn(Set2)))
  data %>% 
    filter(`Protein accession` == toexclude[,1]) %>%
    filter(grepl(species,Organism)) -> SET2
  return(SET2[,c(1,2,7,8,9)])
}

make_interter = function(data,list_of_sample, wanted_sample, species){
  obj = list()
  for (i in 1:length(list_of_sample)){
    obj[[length(obj)+1]] <- get_prot_of_sample(data, list_of_sample[i])
    names(obj)[i] = list_of_sample[i]
  } 
  Set1 = obj[names(obj) %in% wanted_sample]  
  Set2 = obj[!names(obj) %in% wanted_sample]
  toinclude = as.data.frame(RVenn::overlap(Venn(Set1)))
  data %>% 
    filter(sample == wanted_sample) %>%
    filter(`Protein accession` == toinclude[,1]) %>%
    filter(Organism == species) -> SET1
  toexclude = as.data.frame(RVenn::unite(Venn(Set2)))
  data %>% 
    filter(`Protein accession` == toexclude[,1]) %>%
    filter(grepl(species,Organism)) -> SET2
  todiff = as.data.frame(setdiff(SET1,SET2))
  data %>% 
    filter(`Protein accession` == todiff) %>%
    filter(grepl(species,Organism)) %>%
    unique()-> SET3
  return(SET3[,c(1,2,7,8,9)])
}

## Make a heatmap ##

make_heatmap = function(data, listof_sample, species, min, max, norm){ 
  data %>%
    filter(`Mascot score` > min & `Mascot score` < max) %>%
    filter(sample %in% listof_sample) %>%
    pivot_wider(id_cols = c(`Protein accession`,Organism),names_from=sample,values_from=NSAF) -> widedata
  rownames = widedata$`Protein accession`
  organism = widedata$Organism
  organism %>% unique
  widedata %>% dplyr::select(-`Protein accession`, -Organism)  %>% as.matrix -> datamatrix
  datamatrix[organism== species,] 
  rownames(datamatrix) = rownames
  p <- heatmaply(t(datamatrix[organism==species,]), 
                 dendrogram = T,
                 xlab = "", ylab = "", 
                 main = "",
                 scale = norm,
                 margins = c(0,50,NA,0),
                 grid_color = "white",
                 grid_width = 0.0001,
                 titleX = FALSE,
                 hide_colorbar = F,
                 na.value = "grey50",
                 colors = cool_warm,
                 branches_lwd = 0.6,
                 label_names = c("Sample", "Accession", "NSAF"),
                 point_size_name = "NSAF",
                 fontsize_row = 5, fontsize_col = 5,
                 labCol = colnames(t(datamatrix[organism==species,])),
                 labRow = rownames(t(datamatrix[organism==species,])),
                 heatmap_layers = theme(axis.line=element_blank())
  )
  p
}

## Make a NMDS  ##

make_nMDS = function(data, species, score){
  data %>% 
    filter(`#specific peptides`>2) %>% 
    filter(grepl(species,Organism)) %>% 
    dplyr::select(`Protein accession`,sample,SC) %>% 
    pivot_wider(id_cols = `Protein accession`,names_from=sample,values_from=SC) %>% 
    mutate(SCsums = rowSums(across(where(is.numeric)))) %>% filter(SCsums > score) %>%
    dplyr::select(-`Protein accession`,-SCsums) -> data_mds
  metaMDS(data_mds) -> mds
  return(stressplot(mds))
}

make_nMDSbis = function(data, species, score){
  data %>% 
    filter(`#specific peptides`>1) %>% 
    dplyr::filter(grepl(species,Organism)) %>% 
    dplyr::select(`Protein accession`,sample,SC) %>% 
    pivot_wider(id_cols = `Protein accession`,names_from=sample,values_from=SC) %>% 
    mutate(SCsums = rowSums(across(where(is.numeric)))) %>% filter(SCsums > score) %>%
    dplyr::select(-`Protein accession`,-SCsums) -> data_mds
  metaMDS(data_mds) -> mds
  mds$species %>% as.data.frame %>% mutate(sample = rownames(mds$species)) -> MDS
  return(ggplot(MDS, aes(x=MDS1, y=MDS2)) + geom_point(size =3) + 
    geom_label_repel(aes(x=MDS1,y=MDS2,label=sample)) + 
    theme_gray())
}

make_nMDSter = function(data, species, score, supp){
    data %>% 
      filter(`#specific peptides`>1) %>% 
      dplyr::filter(grepl(species,Organism)) %>% 
      dplyr::select(`Protein accession`,sample,SC) %>% 
      pivot_wider(id_cols = `Protein accession`,names_from=sample,values_from=SC) %>% 
      mutate(SCsums = rowSums(across(where(is.numeric)))) %>% filter(SCsums > score) %>%
      dplyr::select(-`Protein accession`,-SCsums) -> data_mds
    metaMDS(data_mds) -> mds
    mds$species %>% as.data.frame %>% mutate(sample = rownames(mds$species)) -> MDS
    return(ggplot(MDS, aes(x=MDS1, y=MDS2, color= unlist(supp[,2]), shape = unlist(supp[,3]))) + 
      geom_point(size=3) + geom_label_repel(aes(x=MDS1,y=MDS2,label=sample))+ 
      labs(shape=colnames(supp[3]),col=colnames(supp)[2]) + theme_gray())

}

## Make a PCA   ##

make_pca_1 = function(data){  
  data %>%
    dplyr::select(NSAF, `Protein accession`,sample,Organism ) %>%
    pivot_wider(names_from = sample, values_from = NSAF) -> cpa_data
  cpa_data[complete.cases(cpa_data), ] -> cpa_data
  row.names(cpa_data) <- cpa_data$`Protein accession`
  PCA(cpa_data, scale.unit=TRUE, ncp=5, quali.sup=c(1:2), graph=F) -> res.pca
  fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
  
}

make_pca_2 = function(data){
  data %>%
    dplyr::select(NSAF, `Protein accession`,sample,Organism ) %>%
    pivot_wider(names_from = sample, values_from = NSAF) -> cpa_data
  cpa_data[complete.cases(cpa_data), ] -> cpa_data
  row.names(cpa_data) <- cpa_data$`Protein accession`
  PCA(cpa_data, scale.unit=TRUE, ncp=5, quali.sup=c(1:2), graph=F) -> res.pca
  return(fviz_pca_var(res.pca, col.var = "contrib", 
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, labelsize = 3))
}

make_pca_3 = function(data, species, list_of_sample, ellipses){
  data %>%
    dplyr::select(NSAF, `Protein accession`,sample,Organism) %>%
    pivot_wider(names_from = sample, values_from = NSAF) -> cpa_data
  row.names(cpa_data) <- cpa_data$`Protein accession`
  PCA(cpa_data, scale.unit=TRUE, ncp=5, quali.sup=c(1:2), graph=F) -> res.pca
  a <-fviz_pca_var(res.pca, col.var = "contrib", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, labelsize = 3)
  b <- fviz_pca_biplot(res.pca,
                       col.ind = cpa_data$Organism, palette = "jco",  
                       addEllipses = ellipses, label = " var", 
                       col.var = a$data$name, repel = TRUE, labelsize = 2,
                       legend.title = "Organisms :")
  ggly <- ggplotly(b)
  bggly <- plotly_build(ggly)
  for (k in 1:length(species)){
    bggly$x$data[[k]]$text <- with(b$data, paste0("Protein: ", name))
  }
  bggly 
}

##################################   UI    #####################################

ui <- dashboardPage(skin ="green", 
                    dashboardHeader(title = tags$a(href='https://umr-astre.cirad.fr/', windowTitle =" Neethling Prot",
                                                   tags$img(src='https://www.gds974.com/wp-content/uploads/2018/02/logo-cirad-avec-texte.jpg',  height='80',width='200')),
                                    tags$li(class="dropdown", tags$a(href="https://github.com/Baud-de-Preval/Dashboard-skeleton/blob/master/ProteomicASTRE.R", icon("github"), "Source Code", target="_blank")),
                                    tags$li(class ="dropdown", tags$a(href ='https://umr-astre.cirad.fr/publications/publications-scientifiques/flux-inra-et-cirad/pub.-agritrop-astre/risk-of-introduction-of-lumpy-skin-disease-in-france-by-the-import-of-vectors-in-animal-trucks', icon("info"))),
                                    tags$li(class = "dropdown", useShinyalert(), dropdownButton(inputId = "contact", label = "",status = "success", icon = icon('envelope')) 
                                            )),
                                            
                    dashboardSidebar(sidebarMenu(
                      menuItem("Welcome page", tabName = "welcome", icon = icon("home")), 
                      menuItem("Data uploader", tabName = "data_upload", icon = icon("upload")),
                      menuItem("Stats", tabName = "stats", icon =icon("eye")),
                      menuItem("Samples intersection", tabName = "inter", icon = icon("exchange"),
                               menuSubItem("Overview", tabName = "view", icon =icon("vcard-o")),
                               menuSubItem("Specific sets", tabName = "set", icon = icon("braille"))),
                      menuItem("Heatmap", tabName = "heatmap", icon = icon("sitemap")),
                      menuItem("Dimensionnal scaling", tabName = "scaling", icon =icon("line-chart"),
                               menuSubItem(text = "nMDS", tabName = "NMDS", icon = icon("area-chart")),
                               menuSubItem("PCA", tabName = "pca", icon = icon("area-chart"))),
                      menuItem("Informations and sources", tabName = "Info", icon =icon("book")),
                      menuItem(downloadButton("report", "Download your report", align = 'center',style="color: #fff; background-color: #228b22; border-color: #32cd32")
                      
                    )
                    )),
                    
                    dashboardBody(tags$head(tags$style(HTML('/* logo */.skin-green .main-header .logo {background-color: #ffffff;}
                                                             /* logo when hovered */.skin-green .main-header .logo:hover {background-color: #ffffff;}'))),
                      tabItems(
                        tabItem(tabName = "welcome",
                                fluidRow(
                                  box(status = "success", width ="100%", align ="center", title = h2("Neethling Prot", align = "center"), tags$hr(),
                                      strong("Welcome !"),"This App will help you to vizualise briefly your results
                                      from your proteomic experiments. From a ", strong("xlsx file"),", you will get a general overview 
                                      on your datas through few classic representations, figures and statistics."),
                                  
                                  box(status = "success", width ="100%", title = h3("Experiments"), tags$hr(), tags$img(), tags$hr(),
                                      "This Shiny App has been developed during a three months internship at", strong("CIRAD"), "concomitantly with a study
                                      on Neethling virus (see paper at this :", tags$a(href ="", "link") ,"). But the App is designed to treat any data obtained from 
                                      mass spectrometry including several organisms. It might still receive updates in the future."))
                        ),
                        tabItem(tabName = "data_upload",
                                fluidRow(
                                  box(status = "success", width = '100%', "You can find  here an ",
                                      tags$a('example', href ="https://github.com/Baud-de-Preval/Dashboard-skeleton/blob/master/data_exemple/Li2D_MS21-001_Table%20S2_List%20of%20Proteins.xlsx"), "of a uploadable file. Once your file have the right format, please upload it :"),
                                
                                         box(status = "success",width = '100%', fileInput(inputId = "filedata",
                                                                                          label = "Choose xlsx file",
                                                                                          accept = c(".xlsx")),
                                             tags$style(".progress-bar {background-color: #5e437;}"),
                                             
                                             tags$hr(),
                                             actionBttn("parse", "Parse dataset for analysis", style ="pill"), align = "center"
                                         ),
                                         box(status = "success", width = '100%', dataTableOutput("contents"), title = "Data overview", solidHeader = T))
                        ),
                        tabItem(tabName = "stats",
                                fluidRow(
                                 box(status = "success", title = "Distribution between samples", width = "80%", plotOutput("Hist"), tags$hr(), align ="center",
                                     downloadBttn('dwlstats', label = "Download Plot", style = "gradient", color = "warning")),
                          
                                 box(status = "success", title = "Settings", solidHeader = T, align = "center", tagList(
                                  radioGroupButtons(
                                    inputId = "num",
                                    label = "Select values :",
                                    choices = c("SC", "NSAF"),
                              selected = "SC"), tags$hr(),
                           strong("Apply log on the y axis :"), switchInput(inputId = "log", onStatus = "success", offStatus = "danger"),
                           )))
                          ),
                        tabItem(tabName = "view",
                                fluidRow(
                                  box(status = "success", title = "Protein in common between samples",width = '100%', solidHeader = T, plotOutput("upsetR"), 
                                      tags$hr(), align ="center", downloadBttn('dwlupset', label = "Download Plot", style = "gradient", color = "warning")),
                                  
                                  box(status = "success", title = "Organism", uiOutput("moreControls")),
                                  
                                  box(status = "success", title = "Order by", align ="center", radioGroupButtons(
                                        inputId = "order",
                                        choices = c("freq", "degree"),
                                        status = "success"), tags$hr(),
                                      switchInput(
                                          inputId = "incr",
                                          value = F,
                                          label = strong("Decreasing"))
                                  ))
                        ),
                        tabItem(tabName = "set",
                                fluidRow(
                                  box(status = "success", title = "Details",width ="100%", solidHeader = T, dataTableOutput("connexion"), tags$hr(), align = 'center',
                                      downloadBttn('dwlset', label = "Download Table", style = "gradient", color = "warning")),
                                  
                                  box(status = "success", title = "Select samples to compare", width = "100%", uiOutput("samplecontrol"), 
                                  tags$hr(), pickerInput("option", label = "View interactions",
                                                         choices = c("Intersection", "Union","Exclusive"),
                                                         choicesOpt = list(icon = c(icon("handshake-o"), icon("random"), icon("exclamation"))))))
                        ),
                        tabItem(tabName = "heatmap",
                                fluidRow(
                                  box(status = "success", title = "Links and differences within samples", plotlyOutput("heat"),tags$hr(),align = 'center'),
                                      
                                  box(status = "success",uiOutput("moreControls_2"), tags$hr(),
                                       noUiSliderInput("limit", "Filter the protein by a minimum Mascot score", 15, 3000, c(2000, 2800)),
                                      tags$hr(), awesomeRadio(
                                        inputId = "normal",
                                        label = " Data transformation by:", 
                                        choices = c("none", "row", "column"),
                                        selected = "none",
                                        inline = TRUE, 
                                        checkbox = F), align ="center"
                                  ))
                                  
                      ),
                      tabItem(tabName = "NMDS",
                              fluidRow(
                                box(status = "success", width = 4, title = "Settings", solidHeader = T, uiOutput("moreControls_3"),
                                    tags$hr(), noUiSliderInput("score", "Filter by a minimum value of reading (SC sums in a column)", 15, 5000, 100),
                                    tags$hr(), " You might add some details about your samples by upload a small file like this", tags$a(href ="https://github.com/Baud-de-Preval/Dashboard-skeleton/blob/master/data_exemple/Supplementary_file.csv" ,"one"),".",
                                    fileInput(inputId = "supp", label ="Add a complementary csv table", accept = c(".csv", ".ods")), align = "center",
                                    actionBttn(inputId = "complement", label = "Add to plot", icon = icon("plus"), style = "gradient", color = "success")), 
                                
                                box(status = "success", title = "Non-metric multidimensionnal scaling", width = 8,
                                    tabBox(height = "100%", width = "100%",
                                           
                                    tabPanel(type = "tabs", title = "Quality of stress optimization", plotOutput("stress"),
                                             tags$hr(),align = 'center',
                                             downloadBttn('dwlstressplot', label = "Download Plot", style = "gradient", color = "warning")
                                    ),
                                    
                                    tabPanel(type = "tabs", title = "Graphical display", plotOutput("MDS"), tags$hr(), align = 'center',
                                             downloadBttn('dwlnMDS', label = "Download Plot", style = "gradient", color = "warning")
                                             )))
                      )),
                      
                      tabItem(tabName = "pca",
                              fluidRow(
                                box(status = "success", width = "100%", tabBox(height = "100%", width = "100%",
                                                               
                                                               tabPanel(type = "tabs", title = "Variance explained by dimensions", plotOutput("barplot"), 
                                                                        tags$hr(),align = 'center',
                                                                        downloadBttn('dwlscreeplot', label = "Download Plot", style = "gradient", color = "warning")
                                                                        ),
                                                               
                                                               tabPanel(type = "tabs", title = "Circle of representation quality", plotOutput("circle"),
                                                                        tags$hr(),align = 'center',
                                                                        downloadBttn('dwlcircle', label = "Download Plot", style = "gradient", color = "warning")
                                                                        ),
                                                               
                                                               tabPanel(type = "tabs", title = "Principal component analysis", plotlyOutput("ACP"),
                                                                        tags$hr(), h4(strong("Display ellipses :")), switchInput("off"),tags$hr(), align = 'center')
                                                                        )))
                      ),
                      
                      tabItem(tabName = "Info",
                              fluidRow(
                                box(status = "success", width ="100%", solidHeader = T, title = "Sources", tags$div(tags$ul(
                                  tags$li(tags$span("Gouveia, D., Pible, O., Culotta, K. et al. Combining proteogenomics and metaproteomics for deep taxonomic and functional characterization of microbiomes from a non-sequenced host. npj Biofilms Microbiomes 6, 23 (2020).",tags$a(href="https://doi.org/10.1038/s41522-020-0133-2","https://doi.org/10.1038/s41522-020-0133-2"))),
                                  tags$li(tags$span("test2")),
                                  tags$li(tags$span("Inspired by : Khan A, Mathelier A. Intervene: a tool for intersection and visualization of multiple gene or genomic region sets. BMC Bioinformatics. 2017;18:287.", tags$a(href ="https://doi.org/10.1186/s12859-017-1708-7", "https://doi.org/10.1186/s12859-017-1708-7"))),
                                  tags$hr(),
                                  strong("More info avalaible at :"), "Alexey I. Nesvizhskii, A survey of computational methods and error rate estimation procedures for peptide and protein identification in shotgun proteomics. 2010.",tags$a(href="https://doi.org/10.1016/j.jprot.2010.08.009", "https://doi.org/10.1016/j.jprot.2010.08.009")))),
                                
                                box(status = "success", title = "Acknowledgment", uiOutput("ack")))
                                
      )
    )
  )                 
)






##################################   Server    #####################################

server <- function(input, output, session){
  
  ## Hide tabs
  

  ## Reactive objects
  
  finaldata = reactiveVal(data.frame())
  samplelist = reactiveVal(c())
  organism = reactiveVal(c())
  otherdata = reactiveVal(data.frame())

  ## Pop-up window
  
  observeEvent(input$contact,{
    shinyalert(title = "Contact us !",html = T, text = "If you have any remarks or questions, please feel free to contact us at : mail@.com", type = "info")
  })

    # Uploading and creating objects
  
  observeEvent(input$parse, {
    finaldata(parse_protein_file(req(input$filedata$datapath)))
    samplelist(finaldata()$sample %>% unique)
    organism(finaldata()$Organism %>% unique)
    output$contents = renderDataTable(finaldata()[,1:8])
  })
  
  output$Hist = renderPlot(
    make_hist(finaldata(), input$num, input$log)
  )
  
  output$moreControls = renderUI({
    tagList(
      awesomeRadio("organismID","Choose species:",organism())
    )
  })
  
  output$upsetR = renderPlot({
    make_upset(finaldata(), samplelist(), input$organismID, input$order, input$incr) 
  })
  
  output$samplecontrol = renderUI({
    tagList(
      awesomeRadio("organismID_4","Choose species:",organism()),
      awesomeCheckboxGroup("control", "Select at least two samples", status = "success", choices = samplelist(), selected = c(samplelist()[1], samplelist()[2]))
    )
  })
  
  output$connexion = renderDataTable({
    if (input$option == "Intersection"){
      make_inter(finaldata(), samplelist(), input$control, input$organismID_4)
    }
    else if (input$option == "Union"){
      make_interbis(finaldata(), samplelist(), input$control, input$organismID_4)
    }
    else {
      make_interter(finaldata(), samplelist(), input$control, input$organismID_4)
    }
  })

  output$moreControls_2 = renderUI({
    tagList(
      awesomeRadio("organismID_2","Choose species:",organism())
    )
  })
  
  output$heat = renderPlotly({
    make_heatmap(finaldata(), samplelist(), input$organismID_2, input$limit[1], input$limit[2], input$normal)
    })
  

  output$moreControls_3 = renderUI({
    tagList(
      awesomeRadio("organismID_3","Choose species:",organism())
    )
  })
  
  output$stress = renderPlot({
    make_nMDS(finaldata(), input$organismID_3, input$score)
  })
    
  observe({
    output$MDS = renderPlot({
      make_nMDSbis(finaldata(), input$organismID_3, input$score)})
    })
  
  observeEvent(input$complement,{
    otherdata(parse_supp_file(input$supp$datapath))
    output$MDS = renderPlot({ 
      make_nMDSter(finaldata(), input$organismID_3, input$score, otherdata())})
  })
  
  output$barplot = renderPlot({
    make_pca_1(finaldata())
  })
  
  output$circle = renderPlot({
    make_pca_2(finaldata())
  })
  
  output$ACP = renderPlotly({
    if (input$off == FALSE){
    make_pca_3(finaldata(), organism(), samplelist(), FALSE)
      }
    else {
    make_pca_3(finaldata(),organism(), samplelist(), TRUE)
    }
  })

  ### Download plots ###
  
  output$dwlstats <- downloadHandler(
    filename = "Histogram.pdf",
    content = function(file){
      device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
      pdf(file)
      make_hist(finaldata(), input$num, input$log)
      dev.off()
    })
  
  output$dwlupset <- downloadHandler(
    filename = "UpsetR.pdf",
    content = function(file){
      device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
      pdf(file)
      make_upset(finaldata(), samplelist(), input$organismID, input$order, input$incr) 
      dev.off()
    })
  
  # Dwl table
  output$dwlset <- downloadHandler(
    filename = "Intersect_file.csv",
    content = function(file){
      device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
      write_csv2(file)
      #function
      dev.off()
    }
  )
  
  output$dwlstressplot <- downloadHandler(
    filename = "Stressplot.pdf",
    content = function(file){
      device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
      pdf(file)
      make_nMDS(finaldata(), input$organismID_3, input$score)
      dev.off()
    })
  
    output$dwlnMDS <- downloadHandler(
    filename = "nMDS.pdf",
    content = function(file){
      device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
      pdf(file)
      make_nMDSbis(finaldata(), input$organismID_3, input$score) 
      observeEvent(input$complement,{
          make_nMDSter(finaldata(), input$organismID_3, input$score, otherdata())})
      dev.off()
    })
    
    output$dwlscreeplot <- downloadHandler(
      filename = "Screeplot.pdf",
      content = function(file){
        device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
        pdf(file)
        make_pca_1(finaldata())
        dev.off()
      })
    
    output$dwlcircle <- downloadHandler(
      filename = "Circle.pdf",
      content = function(file){
        device <- function(..., width, height) grDevices::pdf(..., width = width, height = height, res = 300, units = "in")
        pdf(file)
        make_pca_2(finaldata())
        dev.off()
      },
      contentType = "image/pdf"
      )

  ## Download report
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

####################   App    #######################

shinyApp(ui, server)
