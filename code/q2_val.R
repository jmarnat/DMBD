setwd("/home/koolok/Documents/master/M2/data/archive/big_data_project_confidential")

description2 <- read.csv("BASE_Donnees_Clients.csv", header=TRUE, sep=';')[1,]

clients <- read.csv("BASE_Donnees_Clients.csv", header=TRUE, sep=';')

clients <- clients[c("ID_GRC","TYPOLOGIE")]

reclamation <- read.csv("BASE_Reclamations_clients.csv", header=TRUE, sep=';')

type_reclamation = levels(reclamation$TYPE)

barplot(table(reclamation$TYPE), horiz = FALSE, 
        xlab = "Type of reclamation", ylab = "nb of reclamation",
        names.arg=c(1:9), space = 1)
box()

library(dplyr)

clients_reclamation <- reclamation %>% left_join(clients, by = c("ID_GRC" = "ID_GRC"))