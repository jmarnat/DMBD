setwd(dir = "/home/remi/Documents/M2/DataMiningForBigData/DMBD/big_data_project_confidential/")

library(dplyr)

# upload
data_client <- read.csv("BASE_Donnees_Clients.csv",header = TRUE, sep = ";")

# Create global data frame for satisfaction
data_satisfaction = data.frame()
for (file in list.files(pattern="SAT*")) {
  #print(file)
  if (file == "SATISFACTION_DEMANDE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="demande", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_DENTAIRE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="dentaire_no", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_DENTAIRE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="dentaire", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_OPTIQUE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="optique_no", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_OPTIQUE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="optique", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_SOUSCRIPTION_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="souscritption", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_SUIVI_PROACTIF_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="proactif", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file != "SATISFACTION_RESILIATION_2015_2016_2017.csv"){
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
}

remove(sat)
remove(file)
remove(content)

# modification of the values ID_GRC of client and Meta_donnee.3 of satisfaction
data_client$ID_GRC <- strtoi(data_client$ID_GRC,10)
data_satisfaction$Meta_donnee.3 <- strtoi(data_satisfaction$Meta_donnee.3,10)

# Suppression of the na value
data_satisfaction <- na.omit(data_satisfaction)

# Ordering the dataframe
data_satisfaction <- data_satisfaction[order(data_satisfaction$Meta_donnee.3),]

round2 <- function(x){
  if (x-trunc(x) < 0.25)
    return(trunc(x))
  else if (x-trunc(x) < 0.75)
    return(trunc(x)+0.5)
  else return(trunc(x)+1)
}

# Generation of the dataframe average for an id
average = data.frame()
i <- 1
while (i < length(data_satisfaction$Meta_donnee.3)-1){
  if (data_satisfaction$Meta_donnee.3[i] != data_satisfaction$Meta_donnee.3[i+1]){
    average <- rbind(average,c(data_satisfaction$Q1[i], data_satisfaction$Meta_donnee.3[i],data_satisfaction$Meta_donnee.25[i]))
    i <- i+1
  }
  else {
    nb <- 1
    sum_ <- data_satisfaction$Q1[i]
    while (data_satisfaction$Meta_donnee.3[i] == data_satisfaction$Meta_donnee.3[i+1]){
      sum_ <- sum_ + data_satisfaction$Q1[i+1]
      nb <- nb + 1
      i <- i+1
    }
    average <- rbind(average,c(round2(sum_/nb), data_satisfaction$Meta_donnee.3[i],"Average"))
    i <- i+1
  }
}
colnames(average) <- c("Q1","Meta_donnee.3","Meta_donnee.25")
average$Q1 <- as.double(average$Q1)
average$Meta_donnee.3 <- strtoi(average$Meta_donnee.3,10)

remove(i)
remove(nb)
remove(sum_)

# Mean and standard deviation for Q1
mean_satisfaction <- mean(average$Q1)
std_satisfaction <- sd(average$Q1)
print(mean_satisfaction - std_satisfaction)
print(mean_satisfaction)

# The less satified client
data_insatisfied <- average[average$Q1 < mean_satisfaction - std_satisfaction,]

data_insatisfied <- data_insatisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

# The neutral part
data_neutral <- average[average$Q1 >= mean_satisfaction - std_satisfaction & average$Q1 < mean_satisfaction,]

data_neutral <- data_neutral %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

# The most satified client
data_satisfied <- average[average$Q1 >= mean_satisfaction,]

data_satisfied <- data_satisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

# diagram of the nb of clients according to typology 
par(mar=c(4, 11, 4, 2) + 0.1)
barplot(t(cbind(table(data_insatisfied$TYPOLOGIE),table(data_neutral$TYPOLOGIE),table(data_satisfied$TYPOLOGIE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=tolower(levels(data_insatisfied$TYPOLOGIE)), 
        main = "Level of satisfaction according to TYPOLOGIE")
box()
legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
dev.print(device = png, file = "Level_of_satisfaction_according_to_TYPOLOGIE.png", width = 600)

par(mar=c(2, 3, 3, 1) + 0.1,mfrow = c(1,2))
table_satisfaction = t(cbind(table(data_insatisfied$TYPOLOGIE),table(data_neutral$TYPOLOGIE),table(data_satisfied$TYPOLOGIE)))
for (i in 1:length(table_satisfaction[1,])){
  pie(table_satisfaction[,i], 
        horiz = TRUE, xlab = tolower(levels(data_insatisfied$TYPOLOGIE)[i]), 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        labels=c("","",""),
        #names.arg=tolower(levels(data_insatisfied$TYPOLOGIE)), 
        main = tolower(levels(data_insatisfied$TYPOLOGIE)[i])
  )
  box()
  legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.6,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
  file_name <- paste("Level_of_satisfaction_according_to_TYPOLOGIE",toString(i),".png",sep="")
  dev.print(device = png, file = file_name, width = 600)
}

# diagram of the nb of clients according to ages
par(mar=c(4, 8, 4, 2) + 0.1,mfrow = c(1,1))
barplot(t(cbind(table(data_insatisfied$TRANCHE_AGE),table(data_neutral$TRANCHE_AGE),table(data_satisfied$TRANCHE_AGE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=tolower(levels(data_insatisfied$TRANCHE_AGE)), 
        main = "Level of satisfaction according to TRANCHE AGE")
box()
legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
dev.print(device = png, file = "Level_of_satisfaction_according_to_TRANCHE_AGE.png", width = 600)

par(mar=c(2, 3, 3, 1) + 0.1,mfrow = c(1,2))
table_satisfaction = t(cbind(table(data_insatisfied$TRANCHE_AGE),table(data_neutral$TRANCHE_AGE),table(data_satisfied$TRANCHE_AGE)))
for (i in 1:length(table_satisfaction[1,])){
  pie(table_satisfaction[,i], 
      horiz = TRUE, xlab = tolower(levels(data_insatisfied$TRANCHE_AGE)[i]), 
      col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
      labels=c("","",""), 
      main = tolower(levels(data_insatisfied$TRANCHE_AGE)[i]))
  box()
  legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.6,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
  file_name <- paste("Level_of_satisfaction_according_to_TRANCHE_AGE",toString(i),".png",sep="")
  dev.print(device = png, file = file_name, width = 600)
}

# diagram of the nb of insatisfied clients according to nature
par(mar=c(4, 4, 4, 2) + 0.1, mfrow = c(1,1))
barplot(t(cbind(table(data_insatisfied$NATURE_PERSONNE),table(data_neutral$NATURE_PERSONNE),table(data_satisfied$NATURE_PERSONNE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=levels(data_insatisfied$NATURE_PERSONNE), 
        main = "Level of satisfaction according to NATURE PERSONNE")
box()
legend(x="bottomright", legend=c("< 6","6 -> 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
dev.print(device = png, file = "Level_of_satisfaction_according_to_NATURE_PERSONNE.png", width = 600)

par(mar=c(2, 3, 3, 1) + 0.1,mfrow = c(1,2))
table_satisfaction = t(cbind(table(data_insatisfied$NATURE_PERSONNE),table(data_neutral$NATURE_PERSONNE),table(data_satisfied$NATURE_PERSONNE)))
for (i in 1:length(table_satisfaction[1,])){
  pie(table_satisfaction[,i], 
      horiz = TRUE, xlab = tolower(levels(data_insatisfied$NATURE_PERSONNE)[i]), 
      col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
      labels=c("","",""),
      main = tolower(levels(data_insatisfied$NATURE_PERSONNE)[i]))
  box()
  legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.6,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
  file_name <- paste("Level_of_satisfaction_according_to_NATURE_PERSONNE",toString(i),".png",sep="")
  dev.print(device = png, file = file_name, width = 600)
}

# diagram of the nb of insatisfied clients according to segmentation
par(mar=c(4, 4, 4, 2) + 0.1, mfrow = c(1,1))
barplot(t(cbind(table(data_insatisfied$SEGMENTATION_DISTRIBUTIVE),table(data_neutral$SEGMENTATION_DISTRIBUTIVE),table(data_satisfied$SEGMENTATION_DISTRIBUTIVE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=tolower(levels(data_insatisfied$SEGMENTATION_DISTRIBUTIVE)), 
        main = "Level of satisfaction according to SEGMENTATION DISTRIBUTIVE")
box()
legend(x="bottomright", legend=c("< 6","6 -> 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
dev.print(device = png, file = "Level_of_satisfaction_according_to_SEGMENTATION_DISTRIBUTIVE.png", width = 600)

par(mar=c(2, 3, 3, 1) + 0.1,mfrow = c(1,2))
table_satisfaction = t(cbind(table(data_insatisfied$SEGMENTATION_DISTRIBUTIVE),table(data_neutral$SEGMENTATION_DISTRIBUTIVE),table(data_satisfied$SEGMENTATION_DISTRIBUTIVE)))
for (i in 1:length(table_satisfaction[1,])){
  pie(table_satisfaction[,i], 
      horiz = TRUE, xlab = tolower(levels(data_insatisfied$SEGMENTATION_DISTRIBUTIVE)[i]), 
      col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
      labels=c("","",""),
      main = tolower(levels(data_insatisfied$SEGMENTATION_DISTRIBUTIVE)[i]))
  box()
  legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.6,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
  file_name <- paste("Level_of_satisfaction_according_to_SEGMENTATION_DISTRIBUTIVE",toString(i),".png",sep="")
  dev.print(device = png, file = file_name, width = 600)
}

# diagram of the nb of insatisfied clients according to type of survey
par(mar=c(4, 14, 4, 2) + 0.1, mfrow = c(1,1))
barplot(t(cbind(table(data_insatisfied$Meta_donnee.25),table(data_neutral$Meta_donnee.25),table(data_satisfied$Meta_donnee.25))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=c(tolower(levels(data_satisfaction$Meta_donnee.25)),"average"), 
        main = "Level of satisfaction according to TYPE OF SURVEY")
box()
legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
dev.print(device = png, file = "Level_of_satisfaction_according_to_TYPE_OF_SURVEY.png", width = 600)

par(mar=c(2, 3, 3, 1) + 0.1,mfrow = c(1,2))
table_satisfaction = t(cbind(table(data_insatisfied$Meta_donnee.25),table(data_neutral$Meta_donnee.25),table(data_satisfied$Meta_donnee.25)))
for (i in 1:length(table_satisfaction[1,])){
  pie(table_satisfaction[,i], 
      horiz = TRUE, xlab = tolower(levels(data_insatisfied$Meta_donnee.25)[i]), 
      col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
      labels=c("","",""), 
      main = tolower(levels(data_satisfaction$Meta_donnee.25)[i]))
  box()
  legend(x="topright", legend=c("< 6","6 -> 8",">= 8"), cex=0.6,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")
  file_name <- paste("Level_of_satisfaction_according_to_TYPE_OF_SURVEY",toString(i),".png",sep="")
  dev.print(device = png, file = file_name, width = 600)
}

remove(file_name)
remove(table_satisfaction)
