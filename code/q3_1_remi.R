setwd(dir = "/home/remi/Documents/M2/DataMiningForBigData/DMBD/big_data_project_confidential/")

library(dplyr)

# upload
content <- readLines("SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
data_satisfaction <- read.csv(textConnection(content),header = TRUE, sep = ";")

data_client <- read.csv("BASE_Donnees_Clients.csv",header = TRUE, sep = ";")

# Create global data frame for satisfaction
data_satisfaction = data.frame()
for (file in list.files(pattern="SAT*")) {
  #print(file)
  if (file == "SATISFACTION_DEMANDE_2015_2016_2017.csv") {
    # fichier corrompu trop d'erreurs de csv et bdd
  }
  else if (file == "SATISFACTION_DENTAIRE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    sat <- cbind("Meta_donnee.25"="dentaire_no", sat)
    sat <- cbind("Meta_donnee.28"=FALSE, sat)
    sat <- cbind("Meta_donnee.36"=FALSE, sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
  else if (file == "SATISFACTION_DENTAIRE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    sat <- cbind("Meta_donnee.25"="dentaire", sat)
    sat <- cbind("Meta_donnee.28"=FALSE, sat)
    sat <- cbind("Meta_donnee.36"=FALSE, sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
  else if (file == "SATISFACTION_OPTIQUE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    sat <- cbind("Meta_donnee.25"="optique_no", sat)
    sat <- cbind("Meta_donnee.28"=FALSE, sat)
    sat <- cbind("Meta_donnee.36"=FALSE, sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
  else if (file == "SATISFACTION_OPTIQUE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    sat <- cbind("Meta_donnee.25"="optique", sat)
    sat <- cbind("Meta_donnee.28"=FALSE, sat)
    sat <- cbind("Meta_donnee.36"=FALSE, sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
  else if (file == "SATISFACTION_SOUSCRIPTION_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    sat <- cbind("Meta_donnee.25"="souscritption", sat)
    colnames(sat)[colnames(sat)=="Meta_donnee.28"] <- "Meta_donnee.36"
    colnames(sat)[colnames(sat)=="Meta_donnee.26"] <- "Meta_donnee.28"
    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
  else if (file == "SATISFACTION_SUIVI_PROACTIF_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    sat <- cbind("Meta_donnee.25"="proactif", sat)
    colnames(sat)[colnames(sat)=="Meta_donnee.27"] <- "Meta_donnee.28"
    sat <- cbind("Meta_donnee.36"=FALSE, sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
  else if (file != "SATISFACTION_RESILIATION_2015_2016_2017.csv"){
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')

    data_satisfaction <- rbind(data_satisfaction, sat[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")])
  }
}

remove(sat)
remove(file)
remove(content)

# Modification of the values ID_GRC of client and Meta_donnee.3 of satisfaction
data_client$ID_GRC <- strtoi(data_client$ID_GRC,10)
data_satisfaction$Meta_donnee.3 <- strtoi(data_satisfaction$Meta_donnee.3,10)

# Conversion of the date
data_satisfaction$Date.de.réponse <- as.POSIXct(strptime(data_satisfaction$Date.de.réponse, format="%d/%m/%Y"))
data_satisfaction$Meta_donnee.28 <- as.POSIXct(strptime(data_satisfaction$Meta_donnee.28, format="%d/%m/%Y"))
data_satisfaction$Meta_donnee.36 <- as.POSIXct(strptime(data_satisfaction$Meta_donnee.36, format="%d/%m/%Y"))

# Purge of the uninteresting values
data_satisfaction <- data_satisfaction[c("Date.de.réponse","Q1","Meta_donnee.3","Meta_donnee.25","Meta_donnee.28","Meta_donnee.36")]

# Suppression of the na value
data_satisfaction <- na.omit(data_satisfaction)

# Merge of the two dataframes
data_satisfaction <- data_satisfaction %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

data_satisfaction <- data_satisfaction[order(data_satisfaction$Meta_donnee.3, data_satisfaction$Date.de.réponse),]

# http://forums.cirad.fr/logiciel-R/viewtopic.php?t=663
duplicated2 <- function(x){
  if (sum(dup <- duplicated(x))==0)
    return(dup)
  if (class(x) %in% c("data.frame","matrix"))
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))]
  else duplicated(c(x[dup],x))[-(1:sum(dup))]
}

data_satisfaction <- data_satisfaction[duplicated2(data_satisfaction$Meta_donnee.3),]


