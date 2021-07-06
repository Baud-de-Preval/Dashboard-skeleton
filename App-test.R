library(readr)
library(tidyverse)
library(RColorBrewer)

rawprot <- read_delim("work/R_script/List of Proteins.xlsx - Data_true_Exemple.csv", 
                                                      "\t", escape_double = FALSE, trim_ws = TRUE)

View(rawprot)


Exp <- c("Q27824_MS21-001_H_TCA","Q27825_MS21-001_MH_TCA","Q27826_MS21-001_H","Q27827_MS21-001_MH","Q27828_MS21-001_MB","Q27829_MS21-001_B","Q27830_MS21-001_CulotGrad","Q27831_MS21-001_Culot2Couss","Q27832_MS21-001_B_Holz")
# Tri des variables et manipulation des noms
Sc <- data.frame(select(rawprot, contains("Spectral")))
colnames(Sc) <- c("Q27824_MS21-001_H_TCA","Q27825_MS21-001_MH_TCA","Q27826_MS21-001_H","Q27827_MS21-001_MH","Q27828_MS21-001_MB","Q27829_MS21-001_B","Q27830_MS21-001_CulotGrad","Q27831_MS21-001_Culot2Couss","Q27832_MS21-001_B_Holz")

NSAF <- data.frame(select(rawprot, contains("NSAF")))
colnames(NSAF) <- c("Q27824_MS21-001_H_TCA","Q27825_MS21-001_MH_TCA","Q27826_MS21-001_H","Q27827_MS21-001_MH","Q27828_MS21-001_MB","Q27829_MS21-001_B","Q27830_MS21-001_CulotGrad","Q27831_MS21-001_Culot2Couss","Q27832_MS21-001_B_Holz")

Sc_tot <- rowSums(Sc)
NSAF_tot <- rowSums(NSAF)

ggplot(data = data.frame(Sc_tot), mapping =  aes(fill = NSAF_tot, y=Sc_tot, x= rawprot$Protein)) + 
  theme(axis.text.x = element_text(face="bold", color="black",size=7, angle=45)) +
  geom_bar(position="stack", stat="identity") +
  labs(fill="NSAF") +
  scale_fill_gradient(low="blue", high="red") +
  ggtitle("Raw data from the 9 experiments") + 
  xlab("Protein accession") + 
  ylab("Cumulated spectral count") 

#

ggplot(data = data.frame(Sc_tot), mapping =  aes(fill = NSAF_tot, y=Sc_tot, x= rawprot$Protein)) + 
  theme(axis.text.x = element_text(face="bold", color="black",size=7, angle=45)) +
  geom_bar(position="stack", stat="identity") +
  labs(fill="NSAF") +
  scale_fill_gradient(low="blue", high="red") +
  ggtitle("Raw data from the 9 experiments") + 
  xlab("Protein accession") + 
  ylab("Cumulated spectral count") 





