# setwd(dir = "/home/remi/Documents/M2/DataMiningForBigData/DMBD/big_data_project_confidential/")
setwd(dir = "/home/mathieu/Documents/Master/M2/S3/BigData/Projet/big_data_project_confidential/")

library(dplyr)
library(tm)
library(tokenizers)
rm(list = ls(all = TRUE))
# upload
  content <- readLines("SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
data_satisfaction <- read.csv(textConnection(content),header = TRUE, sep = ";")
data_client <- read.csv("BASE_Donnees_Clients.csv",header = TRUE, sep = ";")
# Create global data frame for satisfaction
  data_satisfaction = data.frame()
for (file in list.files(pattern="SAT*")) {
  #print(file)
    if (file == "SATISFACTION_DEMANDE_2015_2016_2017.csv") {
      content <- readLines(file)[-2]
      sat <- read.csv(textConnection(content), header=TRUE, sep=';')
      sat <- cbind("Meta_donnee.25"="demande", sat)
      data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
      }
  else if (file == "SATISFACTION_DENTAIRE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    sat <- cbind("Meta_donnee.25"="dentaire_no", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  else if (file == "SATISFACTION_DENTAIRE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    sat <- cbind("Meta_donnee.25"="dentaire", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  else if (file == "SATISFACTION_OPTIQUE_NON_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    sat <- cbind("Meta_donnee.25"="optique_no", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  else if (file == "SATISFACTION_OPTIQUE_PARTENAIRE_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    sat <- cbind("Meta_donnee.25"="optique", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  else if (file == "SATISFACTION_SOUSCRIPTION_2015_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    sat <- cbind("Meta_donnee.25"="souscritption", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  else if (file == "SATISFACTION_SUIVI_PROACTIF_2016_2017.csv") {
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    sat <- cbind("Meta_donnee.25"="proactif", sat)
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  else if (file != "SATISFACTION_RESILIATION_2015_2016_2017.csv"){
    content <- readLines(file)[-2]
    sat <- read.csv(textConnection(content), header=TRUE, sep=';')
    data_satisfaction <- rbind(data_satisfaction, sat[c("Q1", "Q2","Meta_donnee.3","Meta_donnee.25")])
    }
  }
remove(sat)
remove(file)
remove(content)
# modification of the values ID_GRC of client and Meta_donnee.3 of satisfaction
  data_client$ID_GRC <- strtoi(data_client$ID_GRC,10)
data_satisfaction$Meta_donnee.3 <- strtoi(data_satisfaction$Meta_donnee.3,10)
# Mean and standard deviation for Q1
  mean_satisfaction <- mean(na.omit(data_satisfaction[,'Q1']))
std_satisfaction <- sd(na.omit(data_satisfaction[,'Q1']))
print(mean_satisfaction - std_satisfaction)
print(mean_satisfaction)
# The less satified client
  data_insatisfied <- na.omit(data_satisfaction[data_satisfaction$Q1 < mean_satisfaction - std_satisfaction,])
data_insatisfied <- data_insatisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))
# The neutral part
  data_neutral <- na.omit(data_satisfaction[data_satisfaction$Q1 >= mean_satisfaction - std_satisfaction & data_satisfaction$Q1 < mean_satisfaction,])
data_neutral <- data_neutral %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))
# The most satified client
  data_satisfied <- na.omit(data_satisfaction[data_satisfaction$Q1 >= mean_satisfaction,])
data_satisfied <- data_satisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))
# colnames(data_insatisfied$Q2) <- c('comment')
  data_insatisfied <- data.frame(data_insatisfied[!is.na(data_insatisfied$Q2) != '',])
# The less satified client
  data_insatisfied <- na.omit(data_satisfaction[data_satisfaction$Q1 < mean_satisfaction - std_satisfaction,])
data_insatisfied <- data_insatisfied %>% left_join(data_client, by = c("Meta_donnee.3" = "ID_GRC"))
# colnames(data_insatisfied$Q2) <- c('comment')
  data_insatisfied <- data.frame(data_insatisfied[data_insatisfied$Q2 != '',])
data_insatisfied <- data.frame(data_insatisfied[!is.na(data_insatisfied$Q2),])
# colnames(data_insatisfied) <- c('comment')
  data_insatisfied <- cbind(row.names(data_insatisfied),data_insatisfied)
colnames(data_insatisfied)[1] <- 'doc_id'
colnames(data_insatisfied)[3] <- 'text'
shorts <- c('j','c','a','y','à','d','n','ere','m','l')
corpus_bad <- Corpus(DataframeSource(data_insatisfied))
corpus_bad <- tm_map(corpus_bad, content_transformer(tolower))
corpus_bad <- tm_map(corpus_bad, removeNumbers)
corpus_bad <- tm_map(corpus_bad, removeWords, c(stopwords('fr'),shorts))
corpus_bad <- tm_map(corpus_bad, removePunctuation)
corpus_bad <- tm_map(corpus_bad, stemDocument, language="fr")
# View(corpus_bad)
tm <- data.frame(corpus_bad$content)
whole_txt = apply(tm,1,paste,collapse=". ")
n_for_grams <- 5
ngrams_tokenizer <- function(x) {
  unlist(tokenize_ngrams(x,lowercase = TRUE, n=6, n_min=3))
  }
ctrls <- list(tokenize = ngrams_tokenizer, stemming=FALSE)
tf <- termFreq(whole_txt, control = ctrls)

most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 20))
# View(most_freq_tf)

remove(caract)

caract<- data.frame()
for(word in rownames(most_freq_tf)){
  wregex = ''
  for( w in unlist(strsplit(word, split=' '))){
    wregex <- paste(wregex,w,'[[:punct:][:alnum:][:blank:]]*',sep = '')
  }
  # print(wregex)
  for(comment in data_insatisfied$text){
    if(grepl(wregex, comment)){
      # print(comment)
      caract <- rbind(caract,data_insatisfied[data_insatisfied$text==comment,c("TRANCHE_AGE","NATURE_PERSONNE","SEGMENTATION_DISTRIBUTIVE","MARCHE_CSP","MARCHE_PSO","TYPOLOGIE")])
    }
  }
}

barplot(t(table(caract$SEGMENTATION_DISTRIBUTIVE, exclude = c('','.','NULL','N'))), 
        horiz = FALSE, ylab = "nb of clients", 
        space = 2, las = 1,
        main = "Comment insatisfaction - SEGMENTATION_DISTRIBUTIVE")
box()


