setwd("/home/koolok/Documents/master/M2/data/archive/big_data_project_confidential")

content <- readLines("SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
sat_auto_bdg <- read.csv(textConnection(content), header=TRUE, sep=';')

description <- read.csv("SATISFACTION_AUTO_BDG_2015_2016_2017.csv", header=TRUE, sep=';')[1,]


content <- readLines("SATISFACTION_DEMANDE_2015_2016_2017.csv")[-2]
sat <- read.csv(textConnection(content), header=TRUE, sep=';')

sat <- cbind("Meta_donnee.25"="demande", sat)

description2 <- read.csv("SATISFACTION_SUIVI_PROACTIF_2016_2017.csv", header=TRUE, sep=';')[1,]





sat_glob = data.frame()

# Create global data frame
for (file in list.files(pattern="SAT*")) {
  print(file)
  if (file == "SATISFACTION_DEMANDE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="demande", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_DENTAIRE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="dentaire_no", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_DENTAIRE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="dentaire", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_OPTIQUE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="optique_no", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_OPTIQUE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="optique", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_SOUSCRIPTION_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="souscritption", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file == "SATISFACTION_SUIVI_PROACTIF_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat <- cbind("Meta_donnee.25"="proactif", sat)
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
  else if (file != "SATISFACTION_RESILIATION_2015_2016_2017.csv"){
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    
    sat_glob <- rbind(sat_glob, sat[c("Q1","Meta_donnee.3","Meta_donnee.25")])
  }
}
  

# Mean and standard deviation for Q1
mean_satis <- mean(na.omit(sat_glob[,'Q1']))
std_satis <- sd(na.omit(sat_glob[,'Q1']))

# Print histogram
hist(sat_glob$Q1)

# Selection of not satisfied
sat_glob_not_sati <- na.omit(sat_glob[sat_glob$Q1<(mean_satis - std_satis),])

hist(sat_glob_not_sati$"Q1")

# Proportion of not satisfied
(nrow(sat_glob_not_sati) / nrow(na.omit(sat_glob))) * 100



# Histogram type
# hist(sat_glob$Meta_donnee.25)
# 
type_event <- levels(sat_glob$Meta_donnee.25) 
# 
levels(sat_glob$Meta_donnee.25) <- c(1:16)
# 
# sat_glob$Meta_donnee.25 <- strtoi(sat_glob$Meta_donnee.25)
# 
# hist(sat_glob$"Meta_donnee.25")
# 
# sum(na.omit(sat_glob$"Meta_donnee.25") == 15)
# type_event[15]




eff_insatis <- c(1:length(type_event))
for (i in 1:length(type_event)) {
  eff_insatis[i] <- sum(sat_glob$Meta_donnee.25 == type_event[i])
}
barplot(table(sat_glob$Meta_donnee.25), horiz = FALSE, 
        xlab = "TRANCHE_AGE", ylab = "nb of clients",
        names.arg=c(1:16), space = 2)
box()


sum(sat_glob$Meta_donnee.25 == type_event[3])

table(sat_glob$Meta_donnee.25)

