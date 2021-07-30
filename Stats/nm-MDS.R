### Non-metric MDS & Venn Diagram

library(magrittr)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(readxl)
library(MASS)
library(janitor)
library(VennDiagram)

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
as.data.frame(table(finaldata$`Protein accession`)) %>% dplyr::filter(Freq >= round(0.90*length(samples))) -> finaldata75
finaldata %>% filter(`Protein accession` %in% finaldata75$Var1) -> finaldata75

# Diagramme de Venn

Species <-as.list(finaldata %>% count(Organism) %>% unique)

finaldata %>% filter(Organism == Species[1]) -> x_data
finaldata %>% filter(Organism == Species[2]) -> y_data

if (length(x_data) > length(y_data)){
  select(y_data$`Protein accession` %in% x_data$`Protein accession` ) -> Com_data
  
} else {
  select(x_data$`Protein accession` %in% y_data$`Protein accession` ) -> Com_data
}


finaldata$`Protein accession` %>% intersect(x = x_data, y = y_data) %>% filter() -> Com_data


nb = dplyr::filter(as.data.frame(x_data$`Protein accession`)) %>% unique %>% count
nl = as.data.frame(y_data$`Protein accession`) %>% filter %>% unique %>% count
n = as.data.frame(Com_data$`Protein accession`) %>% filter %>% unique %>% count

grid.newpage()
draw.pairwise.venn(nb, nl, n, category = c(Species), lty = rep("blank", 2),
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0, 0), cat.dist = rep(0.025, 2), scaled = FALSE)

# Tentative MDS - Brad Curtis

mds <- as.matrix(finaldata75[,16:19])   # Puissance insuffisante
d <- dist(mds)
fit <- isoMDS(d, k = 2)

colnames(mds) <- c("Dim.1", "Dim.2")

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata), cex=.7) 

# Tentative MDS - Jaccard

mds <- as.matrix(finaldata[,16:19])

      #weighted jaccard similarity matrix setup
sim.jac <- matrix(0, nrow=nrow(mds), ncol=nrow(mds))
rownames(sim.jac) <- rownames(mds)
colnames(sim.jac) <- rownames(mds)

      #weighted jaccard function
pairs <- t(combn(1:nrow(mds), 2))
for (i in 1:nrow(pairs)){
  num <- sum(sapply(1:ncol(mds), function(x)(min(mds[pairs[i,1],x],mds[pairs[i,2],x]))))
  den <- sum(sapply(1:ncol(mds), function(x)(max(mds[pairs[i,1],x],mds[pairs[i,2],x]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}
sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

      #weighted jaccard distance
dist.jac <- 1-sim.jac
