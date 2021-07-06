# Vérification du nb de protéines

library(readxl)
library(tidyverse)
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
df <- df[,-24]
df <- separate(df,Origine_Query, "Query", sep = 6)
df_order <- df %>% group_by(Accession) %>% top_n(1, 'Mascot Peptide Score')
df_uni <- distinct(summarise(df_order)) # --> Nb correspondant au tableau des protéines

df_LSD <- distinct(dplyr::filter(df_order, grepl('Lumpy skin disease virus', Species))) 
df_Bos <- distinct(dplyr::filter(df_order, grepl('Bos taurus]', Species)))
summarise(df_LSD)
summarise(df_Bos) # On retrouve les données de 1.1


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

ggplot(data = V,  mapping = aes(y= N, x= Sample, fill = colSums.df_untidy...22.30..)) + # Le graphe n'affiche aucune donnée
  theme(axis.text.x = element_text(face="bold", color="black",angle = 32)) +
  ggtitle("Number of protein in each sample") + 
  xlab("Sample") + 
  ylab("Cumulated number of protein") 

## Protéines trouvées dans chaque échantillon
# Pour chaque échantillon, nb de protéine et de peptide qui les forment

Nb_pep<-matrix(rep(0,36), ncol=9, nrow=4, byrow=FALSE) # Création d'une matrice vierge
colnames(Nb_pep) = Sample
rownames(Nb_pep) = c("1 peptide", "2 peptides", "3 peptides", "plus de 3 peptides")

df %>% group_by(sequence) %>% top_n(1, `Mascot Peptide Score`) # Gérer les doublons avec max(MPS)

for (k in 1:9){
  df_3 <- filter(df, Query == Sample[k])
  for (l in table(factor(df_3$sequence))){
    return(l)
    if (l <= 3){
    Nb_pep[l,k] = Nb_pep[l,k] +1
    }
    else {
      Nb_pep[4,k] =Nb_pep[l,k] +1
    } 
  }
}

 # Pour l'instant renvoie une matrice Nb_pep incorrecte

ggplot(data = V,  mapping = aes(y= N, x= Sample, fill = colSums.df_untidy...22.30..)) + 
  theme(axis.text.x = element_text(face="bold", color="black",angle = 32)) +
  ggtitle("Number of protein in each sample") + 
  xlab("Sample") + 
  ylab("Cumulated number of protein") +
  facet_wrap(~Sample)

###

# Trouver un moyen de compter le nb de protéine unique pour chaque échantillon

nrow(distinct(dplyr::filter(df_untidy2, grepl('Lumpy skin disease virus', Accession))) )
nrow(is.na(df_untidy2$Q27824))

ggplot(data = data.frame(Sc_tot), mapping =  aes(fill = NSAF_tot, y=Sc_tot, x= rawprot$Protein)) + 
  theme(axis.text.x = element_text(face="bold", color="black",size=7, angle=45)) +
  geom_bar(position="stack", stat="identity") +
  labs(fill="NSAF") +
  scale_fill_gradient(low="blue", high="red") +
  ggtitle("Raw data from the 9 experiments") + 
  xlab("Protein accession") + 
  ylab("Cumulated spectral count") 

# Garder les scores supérieurs à un seuil fixé

pep_clear <- pep_uni$`Mascot Peptide Score` > 40
summary(pep_clear)
pep_clean <- pep_uni[pep_clear,]

pep_virus <- pep_clean %>% filter(Species(starts_with("Lumpy")))




