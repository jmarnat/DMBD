library(arules)
# install.packages('arules')


setwd('~/Documents/MLBD/Project/')

content <- readLines("big_data_project_confidential/SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
sat_auto_bdg <- read.csv(textConnection(content), header=TRUE, sep=';')

levels(sat_auto_bdg[,4]) <- 

content <- readLines("big_data_project_confidential/SATISFACTION_AUTO_CLASSIQUE_2015_2016_2017.csv.csv")[-2]
sat_auto_classique <- read.csv(textConnection(content), header=TRUE, sep=';')
