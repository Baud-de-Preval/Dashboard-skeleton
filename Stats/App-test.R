# Vérification du nb de protéines

library(readxl)
library(tidyverse)
library(reshape2)
library(plyr)

pep1 <- read_excel("Téléchargements/Li2D_MS21-001_Table S1_List of PSMs.xlsx")
pep <- pep1[,-12]

summary(pep)

# Supprimer les doublons pour garder les accessions uniques avec max(Mascot seq score) --> A discuter

# 1.1 Tableau des protéines

pep_order <- pep[order(pep$`sequence`, decreasing = T), ]
pep_uni <- pep_order %>% group_by(Accession) %>% top_n(1, `Mascot Peptide Score`)
summarise(pep_uni) 

distinct(dplyr::filter(pep_uni, grepl('Lumpy skin disease virus]', Species)), Species) # --> 125 avec les parties grisées (114 retenues)
distinct(dplyr::filter(pep_uni, grepl('Bos taurus]', Species)), Species) # 1669 --> 73 entrées en trop ?

# 1.2 Tableau des peptides

df <- read_excel("work/R_script/Peptide representation.xlsx")
df %>% unite("description", 22:24,remove = T) -> df
df <- separate(df,Origine_Query, "Query", sep = 6)
df_order <- df %>% group_by(Accession) %>% top_n(1, 'Mascot Peptide Score')
df_uni <- distinct(summarise(df_order)) # --> Nb correspondant au tableau des protéines

df_untidy <- df %>% pivot_wider(names_from = Query, values_from = `Mascot Peptide Score`)
df_untidy <- mutate_at(df_untidy, colnames(df_untidy[,22:30]), ~replace(., is.na(.), 0))
df_sum <- data.frame(colSums(df_untidy[,22:30]))

# Représenter les données en fonction des échantillons

df_untidy2 <- df %>% pivot_wider(names_from = Query, values_from = Accession)
df2 <- relocate(df_untidy2,'Q27824', 'Q27825', 'Q27826', 'Q27827', 'Q27828', 'Q27829', 'Q27830', 'Q27831', 'Q27832', 'Mascot Peptide Score','sequence')

x1 <- length(table(factor(df2$Q27824)))
x2 <- length(table(factor(df2$Q27825)))
x3 <- length(table(factor(df2$Q27826)))
x4 <- length(table(factor(df2$Q27827)))
x5 <- length(table(factor(df2$Q27828)))
x6 <- length(table(factor(df2$Q27829)))
x7 <- length(table(factor(df2$Q27830)))
x8 <- length(table(factor(df2$Q27831)))
x9 <- length(table(factor(df2$Q27832)))

N <- c(x1,x2,x3,x4,x5,x6,x7,x8,x9) # Nombre de protéine détectée dans tous les échantillons
Sample <- row.names(df_sum)
V <- data.frame(N, colSums(df_untidy[,22:30]))

V %>% ggplot(mapping = aes(y= N, x= Sample, color = V[,2])) +
  geom_bar(fill = "steelblue", stat = "identity") +
  labs(color="Mascot score") +
  scale_color_gradient(low="blue", high="red") +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_text(face="bold", color="black",angle = 32)) +
  ggtitle("Number of protein in each sample") + 
  xlab("Sample") + 
  ylab("Cumulated number of protein") 

## Protéines trouvées dans chaque échantillon
# Pour chaque échantillon, nb de protéine et de peptide qui les forment

Nb_pep<-matrix(rep(0,36), ncol=9, nrow=5, byrow=FALSE) # Création d'une matrice vierge
colnames(Nb_pep) = Sample
name = c("1 peptide", "2 to 5 peptides", "5 to 20 peptides", " 20 to 50 peptides","more than 50 peptides")
rownames(Nb_pep)<- name
df3 <- df %>% group_by(Accession, Query, description) %>% 
  select(sequence) %>% unique 

i=0
for (l in 1:9){
  i=i+1
  df4 <- subset(df3,Query == Sample[l])
  nb = as.numeric(table(df4$Accession))
  Nb_pep[1,i] <- sum(nb==1)
  Nb_pep[2,i] <- sum(nb>1 & nb<= 5)
  Nb_pep[3,i] <- sum(nb>5 & nb<=20)
  Nb_pep[4,i] <- sum(nb>20 & nb<=50)
  Nb_pep[5,i] <- sum(nb>50)
}
Nb_pep
NB <-data.frame(Nb_pep)
NB <- pivot_longer(NB, Sample, names_to = "Trial", values_to = "Number")

p = ggplot(data = data.frame(NB), mapping = aes(y= unlist(NB[,2]), x= rep(rownames(Nb_pep),9), levels = rep(name, 9)), fill =Trial) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(face="bold", color="black", size = 6)) +
  geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25, size = 2.5) + 
  ggtitle("Number of peptide composing proteins") + 
  ylab("Protein composition")+ 
  theme(legend.position="none") +
  xlab("")+
  facet_wrap(~ Trial, scales = "free_x") +
  aes(fill = as.factor(Trial)) 
p

###
# Nombre de protéines par échantillons et par type

df5 <- df3 %>% mutate("Organism")

LSD <- sapply(df5, grep, pattern = "Lumpy")
Bos2 <- matrix(sapply(df5, grep, pattern = "ovin"))
Bos1 <- matrix(sapply(df5, grep, pattern = "taurus"))
Bos <- join(Bos1, Bos2)
# Comparer les échantillons par ACM, ACP ?


nrow(distinct(dplyr::filter(df_untidy2, grepl('Lumpy skin disease virus', Accession))) )
nrow(is.na(df_untidy2$Q27824))

