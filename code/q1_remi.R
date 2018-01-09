setwd(dir = "/home/remi/Documents/M2/DataMiningForBigData/Project: data mining for big data")

content <- readLines("big_data_project_confidential/SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
data <- read.csv(textConnection(content),header = TRUE, sep = ";")

# discretisation of the values
for (i in 4:11) {
  levels(data[,i])<- c(NaN,1,2,3,0)
}
for (i in 12:15) {
  levels(data[,i]) <- c(NaN,0,1)
}

# Mean and standard deviation for Q1
mean_satis <- mean(na.omit(data[,'Q1']))
std_satis <- sd(na.omit(data[,'Q1']))
