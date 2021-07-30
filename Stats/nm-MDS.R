### Non-metric MDS

library(magrittr)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(readxl)
library(MASS)
library(janitor)
library(VennDiagram)
library(vegan)

samples = readxl::read_xlsx("Téléchargements/Li2D_MS21-001_Table S2_List of Proteins(1).xlsx",
                            skip=17,n_max=1,col_names = F)
length = dim(samples)[[2]]
samples = samples %>%  dplyr::select(seq(from = 11, to = length,by = 3))
samples = t(samples)[,1]
names(samples) = NULL


data = readxl::read_xlsx("Téléchargements/Li2D_MS21-001_Table S2_List of Proteins(1).xlsx",
                         skip=19,col_names = T)
global_data = data %>% dplyr::select(1:15)
colnames(global_data)[7]="BestSpecificSample"


finaldata = data.frame()
for(i in 1:length(samples)){
  print(i)
  first = 16+(i-1)*3
  last = 16+i*3-1
  data %>% dplyr::select(first:last) -> tmp_data
  colnames(tmp_data)= c("SC","NSAF","%NSAF")
  tmp_global = global_data
  tmp_global$sample = samples[i]
  tmp = cbind(tmp_global,tmp_data)
  finaldata = rbind(finaldata,tmp)
}

# Réduction des données aux prot > 75%

finaldata %>% filter(finaldata$SC != 0) -> finaldata
as.data.frame(table(finaldata$`Protein accession`)) %>% dplyr::filter(Freq >= round(0.75*length(samples))) -> finaldata75
finaldata %>% filter(`Protein accession` %in% finaldata75$Var1) -> finaldata75


mds <- tibble(finaldata75[,17:19])
rownames(mds) <- make.names(finaldata75[,16], unique = TRUE)
Prot.mds <- metaMDS(comm = mds, distance = "bray", trace = FALSE, autotransform = FALSE)



### Diagramme de Venn

# Récupération des espèces
Species <-as.vector(finaldata %>% count(Organism) %>% unique)[,1]
finaldata %>% filter(Organism == Species[1]) -> x_data
finaldata %>% filter(Organism == Species[2]) -> y_data

# Protéines en communs
finaldata$`Protein accession` %>% intersect(x = x_data, y = y_data) %>% filter() -> Com_data

nb = dplyr::filter(as.data.frame(x_data$`Protein accession`)) %>% unique %>% count
nl = as.data.frame(y_data$`Protein accession`) %>% filter %>% unique %>% count
n = as.data.frame(Com_data$`Protein accession`) %>% filter %>% unique %>% count

# Affichage
grid.newpage()
draw.pairwise.venn(nb, nl, n, category = c(Species[1], Species[2]), lty = rep("blank", 2),
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0, 0), cat.dist = rep(0.05, 2), scaled = FALSE)




