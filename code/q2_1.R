setwd("/home/koolok/Documents/master/M2/data/archive/big_data_project_confidential")
library(dplyr)

#description2 <- read.csv("BASE_Donnees_Clients.csv", header=TRUE, sep=';')[1,]

clients <- read.csv("BASE_Donnees_Clients.csv", header=TRUE, sep=';')

clients <- clients[c("ID_GRC","TYPOLOGIE","MARCHE_PSO","COD_INSEE")]

clients$COD_INSEE = gsub("\\d\\d\\d$","",clients$COD_INSEE)

reclamation <- read.csv("BASE_Reclamations_clients.csv", header=TRUE, sep=';')
reclamation <- filter(reclamation, grepl("^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$", ID_GRC, perl=TRUE))

reclamation$ID_GRC <- strtoi(reclamation$ID_GRC,10)
clients$ID_GRC <- strtoi(clients$ID_GRC,10)


# Number of reclamation in function of the type
type_reclamation = levels(reclamation$TYPE)

table_reclamation_type = table(reclamation$TYPE)
table_reclamation_type = sort(table_reclamation_type, decreasing=TRUE)

levels_reclamation_type = tolower(rownames(table_reclamation_type))

for (i in 1:length(levels_reclamation_type)) {
  levels_reclamation_type[i] = gsub("reclamation ","",levels_reclamation_type[i])
}

pie(table_reclamation_type, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the TYPE")
box()
legend(x="topright", legend=levels_reclamation_type, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_TYPE.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Joint the table reclamation with the client database
clients_reclamation <- reclamation %>% left_join(clients, by = c("ID_GRC" = "ID_GRC"))

clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "08")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "09")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "22")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "32")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "55")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "65")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "81")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "90")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "98")
clients_reclamation[nrow(clients_reclamation) + 1,] = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "99")

# Histogram typologie
table_reclamation_typologie = table(clients_reclamation$TYPOLOGIE)
table_reclamation_typologie = sort(table_reclamation_typologie, decreasing=TRUE)

levels_reclamation_typologie = tolower(rownames(table_reclamation_typologie))

pie(table_reclamation_typologie, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the client typologie")
box()
legend(x="topright", legend=levels_reclamation_typologie, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_client_typologie.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Histogram typologie proportion
table_reclamation_typologie = table(clients_reclamation$TYPOLOGIE) / table(clients$TYPOLOGIE)
table_reclamation_typologie = sort(table_reclamation_typologie, decreasing=TRUE)

levels_reclamation_typologie = tolower(rownames(table_reclamation_typologie))

pie(table_reclamation_typologie, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the client typologie\n in proportion of client of this categorie")
box()
legend(x="topright", legend=levels_reclamation_typologie, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_client_typologie_in_proportion.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Histogram profile
table_reclamation_pso = table(clients_reclamation$MARCHE_PSO)
table_reclamation_pso = sort(table_reclamation_pso, decreasing=TRUE)

levels_reclamation_pso = tolower(rownames(table_reclamation_pso))

pie(table_reclamation_pso, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the client MARCHE_PSO")
box()
legend(x="topright", legend=levels_reclamation_pso, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_client_MARCHE_PSO.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Histogram profile proportion
table_reclamation_pso = table(clients_reclamation$MARCHE_PSO) / table(clients$MARCHE_PSO)
table_reclamation_pso = sort(table_reclamation_pso, decreasing=TRUE)

levels_reclamation_pso = tolower(rownames(table_reclamation_pso))

pie(table_reclamation_pso, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the client MARCHE_PSO\n in proportion of client of this categorie")
box()
legend(x="topright", legend=levels_reclamation_pso, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_client_MARCHE_PSO_proportion.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Agricole
agricole = clients_reclamation[ clients_reclamation$MARCHE_PSO=="AGRICOLE",]

table_reclamation_type = table(agricole$TYPE)
table_reclamation_type = sort(table_reclamation_type, decreasing=TRUE)

levels_reclamation_type = tolower(rownames(table_reclamation_type))

for (i in 1:length(levels_reclamation_type)) {
  levels_reclamation_type[i] = gsub("reclamation ","",levels_reclamation_type[i])
}

pie(table_reclamation_type, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of \"agricole\" reclamation in fuction of the TYPE")
box()
legend(x="topright", legend=levels_reclamation_type, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_agricole_reclamation_in_fuction_of_the_TYPE.png",sep="")
dev.print(device = png, file = file_name, width = 600)



# Reclamation Departement
table_reclamation_departement = table(clients_reclamation$COD_INSEE)
table_reclamation_departement = sort(table_reclamation_departement, decreasing=TRUE)[1:9]


levels_reclamation_departement = tolower(rownames(table_reclamation_departement))

pie(table_reclamation_departement, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the client \'departement\'")
box()
legend(x="topright", legend=levels_reclamation_departement, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_client_departement.png",sep="")
dev.print(device = png, file = file_name, width = 600)

# Reclamation Departement proportion
table_reclamation_departement = table(clients_reclamation$COD_INSEE) / table(clients$COD_INSEE)
table_reclamation_departement = sort(table_reclamation_departement, decreasing=TRUE)[1:9]


levels_reclamation_departement = tolower(rownames(table_reclamation_departement))

pie(table_reclamation_departement, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of reclamation in fuction of the client \'departement\' \n in proportion of client of this categorie")
box()
legend(x="topright", legend=levels_reclamation_departement, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_reclamation_in_fuction_of_the_client_departement_proportion.png",sep="")
dev.print(device = png, file = file_name, width = 600)









# Resiliation
content <- readLines("SATISFACTION_RESILIATION_2015_2016_2017.csv")[-2]
resiliation <- read.csv(textConnection(content), header=TRUE, sep=';')

resiliation$Meta_donnee.3 = strtoi(resiliation$Meta_donnee.3)
clients_resiliation <- resiliation %>% left_join(clients, by = c("Meta_donnee.3" = "ID_GRC"))

# Histogram typologie
table_resiliation_typologie = table(clients_resiliation$TYPOLOGIE)
table_resiliation_typologie = sort(table_resiliation_typologie, decreasing=TRUE)

levels_resiliation_typologie = tolower(rownames(table_resiliation_typologie))

pie(table_resiliation_typologie, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of resiliation in fuction of the client typologie")
box()
legend(x="topright", legend=levels_resiliation_typologie, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_resiliation_in_fuction_of_the_client_typologie.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Histogram typologie proportion
table_resiliation_typologie = table(clients_resiliation$TYPOLOGIE) / table(clients$TYPOLOGIE)
table_resiliation_typologie = sort(table_resiliation_typologie, decreasing=TRUE)

levels_resiliation_typologie = tolower(rownames(table_resiliation_typologie))

pie(table_resiliation_typologie, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of resiliation in fuction of the client typologie\n in proportion of client of this categorie")
box()
legend(x="topright", legend=levels_resiliation_typologie, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_resiliation_in_fuction_of_the_client_typologie_proportion.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Histogram profile
table_resiliation_pso = table(clients_resiliation$MARCHE_PSO)
table_resiliation_pso = sort(table_resiliation_pso, decreasing=TRUE)

levels_resiliation_pso = tolower(rownames(table_resiliation_pso))

pie(table_resiliation_pso, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of resiliation in fuction of the client MARCHE_PSO")
box()
legend(x="topright", legend=levels_resiliation_pso, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_resiliation_in_fuction_of_the_client_MARCHE_PSO.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Histogram profile proportion
table_resiliation_pso = table(clients_resiliation$MARCHE_PSO) / table(clients$MARCHE_PSO)
table_resiliation_pso = sort(table_resiliation_pso, decreasing=TRUE)

levels_resiliation_pso = tolower(rownames(table_resiliation_pso))

pie(table_resiliation_pso, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of resiliation in fuction of the client MARCHE_PSO\n in proportion of client of this categorie")
box()
legend(x="topright", legend=levels_resiliation_pso, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_resiliation_in_fuction_of_the_client_MARCHE_PSO_proportion.png",sep="")
dev.print(device = png, file = file_name, width = 600)


# Resiliation departement
table_resiliation_departement = table(clients_resiliation$COD_INSEE)
table_resiliation_departement = sort(table_resiliation_departement, decreasing=TRUE)[1:9]


levels_resiliation_departement = tolower(rownames(table_resiliation_departement))

pie(table_resiliation_departement, 
    horiz = TRUE, 
    col=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"), space = 2, las = 1,
    labels=c("","",""), 
    main = "Number of resiliation in fuction of the client \'departement\'")
box()
legend(x="topright", legend=levels_resiliation_departement, cex=0.8,fill=c("#F0F8FF","#FAEBD7","#00FFFF","#8A2BE2","#A52A2A","#5F9EA0","#7FFF00","#FF7F50","#9ABCDE"),bty="n")
file_name <- paste("Number_of_resiliation_in_fuction_of_the_client_departement.png",sep="")
dev.print(device = png, file = file_name, width = 600)

