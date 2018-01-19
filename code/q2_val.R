setwd("/home/koolok/Documents/master/M2/data/archive/big_data_project_confidential")
library(dplyr)

#description2 <- read.csv("BASE_Donnees_Clients.csv", header=TRUE, sep=';')[1,]

clients <- read.csv("BASE_Donnees_Clients.csv", header=TRUE, sep=';')

clients <- clients[c("ID_GRC","TYPOLOGIE","MARCHE_PSO")]

reclamation <- read.csv("BASE_Reclamations_clients.csv", header=TRUE, sep=';')
reclamation <- filter(reclamation, grepl("^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$", ID_GRC, perl=TRUE))


type_reclamation = levels(reclamation$TYPE)

barplot(table(reclamation$TYPE), horiz = FALSE, 
        xlab = "Type of reclamation", ylab = "nb of reclamation",
        names.arg=c(1:9), space = 1)
box()

clients_reclamation <- reclamation %>% left_join(clients, by = c("ID_GRC" = "ID_GRC"))

# Histogram typologie
typologie <- levels(clients_reclamation$TYPOLOGIE)
barplot(table(clients_reclamation$TYPOLOGIE), horiz = FALSE, 
        xlab = "Typologie", ylab = "nb of reclamation",
        names.arg=c(1:6), space = 1)
box()

# Histogram typologie proportion
barplot(table(clients_reclamation$TYPOLOGIE) / table(clients$TYPOLOGIE), horiz = FALSE, 
        xlab = "Typologie", ylab = "nb of reclamation",
        names.arg=c(1:6), space = 1)
box()


# Histogram profile
profile <- levels(clients$MARCHE_PSO)
barplot(table(clients_reclamation$MARCHE_PSO), horiz = FALSE, 
        xlab = "Typologie", ylab = "nb of reclamation",
        names.arg=c(1:7), space = 1)
box()


# Histogram profile proportion
barplot(table(clients_reclamation$MARCHE_PSO) / table(clients$MARCHE_PSO), horiz = FALSE, 
        xlab = "Profile", ylab = "nb of reclamation",
        names.arg=c(1:7), space = 1)
box()


profile[2]
























