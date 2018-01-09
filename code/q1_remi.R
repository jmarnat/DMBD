setwd(dir = "/home/remi/Documents/M2/DataMiningForBigData/DMBD")

library(dplyr)

# upload
content <- readLines("big_data_project_confidential/SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
data_satisfaction <- read.csv(textConnection(content),header = TRUE, sep = ";")

data_client <- read.csv("big_data_project_confidential/BASE_Donnees_Clients.csv",header = TRUE, sep = ";")

# discretisation of the values
for (i in 4:11) {
  levels(data_satisfaction[,i])<- c(NaN,1,2,3,0)
}
for (i in 12:15) {
  levels(data_satisfaction[,i]) <- c(NaN,0,1)
}

# modification of the values ID_GRC of client
data_client$ID_GRC <- strtoi(data_client$ID_GRC,10)

# Mean and standard deviation for Q1
mean_satisfaction <- mean(na.omit(data_satisfaction[,'Q1']))
std_satisfaction <- sd(na.omit(data_satisfaction[,'Q1']))

# The less satified client
data_insatisfied <- na.omit(data_satisfaction[data_satisfaction$Q1 < mean_satisfaction - std_satisfaction,])

data_insatisfied <- data_insatisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

# The neutral part
data_neutral <- na.omit(data_satisfaction[data_satisfaction$Q1 >= mean_satisfaction - std_satisfaction & data_satisfaction$Q1 < mean_satisfaction,])

data_neutral <- data_neutral %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

# The most satified client
data_satisfied <- na.omit(data_satisfaction[data_satisfaction$Q1 >= mean_satisfaction,])

data_satisfied <- data_satisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))

# diagram of the nb of clients according to typology 
barplot(t(cbind(table(data_insatisfied$TYPOLOGIE),table(data_neutral$TYPOLOGIE),table(data_satisfied$TYPOLOGIE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=substr(levels(data_insatisfied$TYPOLOGIE),1,10), 
        main = "Level of satisfaction according to TYPOLOGIE")
box()
legend(x="topright", legend=c("< 6","6 - 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")

# diagram of the nb of clients according to ages
barplot(t(cbind(table(data_insatisfied$TRANCHE_AGE),table(data_neutral$TRANCHE_AGE),table(data_satisfied$TRANCHE_AGE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=substr(levels(data_insatisfied$TRANCHE_AGE),1,11), 
        main = "Level of satisfaction according to TRANCHE AGE")
box()
legend(x="topright", legend=c("< 6","6 - 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")

# diagram of the nb of insatisfied clients according to nature
barplot(t(cbind(table(data_insatisfied$NATURE_PERSONNE),table(data_neutral$NATURE_PERSONNE),table(data_satisfied$NATURE_PERSONNE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=levels(data_insatisfied$NATURE_PERSONNE), 
        main = "Level of satisfaction according to NATURE PERSONNE")
box()
legend(x="bottomright", legend=c("< 6","6 - 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")

# diagram of the nb of insatisfied clients according to segmentation
barplot(t(cbind(table(data_insatisfied$SEGMENTATION_DISTRIBUTIVE),table(data_neutral$SEGMENTATION_DISTRIBUTIVE),table(data_satisfied$SEGMENTATION_DISTRIBUTIVE))), 
        horiz = TRUE, xlab = "nb of clients", 
        col=c("#F5BCA9","#F7D358","#D8F781"), space = 2, las = 1,
        names.arg=levels(data_insatisfied$SEGMENTATION_DISTRIBUTIVE), 
        main = "Level of satisfaction according to SEGMENTATION DISTRIBUTIVE")
box()
legend(x="bottomright", legend=c("< 6","6 - 8",">= 8"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781"),bty="n")

