if (FALSE) {
  install.packages('tm')
  install.packages('tokenizers')
}

library(tm)
library(tokenizers)


setwd('~/Documents/DMBD/DMBD')

content <- readLines("big_data_project_confidential/SATISFACTION_AUTO_BDG_2015_2016_2017.csv")[-2]
sat_auto_bdg <- read.csv(textConnection(content), header=TRUE, sep=';')


content <- readLines("big_data_project_confidential/SATISFACTION_AUTO_CLASSIQUE_2015_2016_2017.csv")[-2]
sat_auto_classique <- read.csv(textConnection(content), header=TRUE, sep=';')


note_bad <- 3
bad <- data.frame(sat_auto_classique[sat_auto_classique$Q1 < note_bad,]$Q2)
colnames(bad) <- c('comment')
bad <- data.frame(bad[bad$comment != '',])
colnames(bad) <- c('comment')
bad <- data.frame(bad[!is.na(bad$comment),])
colnames(bad) <- c('comment')
bad <- cbind(row.names(bad),bad)
colnames(bad) <- c('doc_id','text')

corpus_bad <- Corpus(DataframeSource(bad))

tm <- tm_map(corpus_bad, removeWords, stopwords("french"))
whole_txt = apply(data.frame(tm$content),1,paste,collapse=". ")
controls <- list(removePunctuation=TRUE, tokenize = 'Boost',removeNumbers=TRUE) 
tf <- (termFreq(whole_txt, control = controls))
most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 100))
View(most_freq_tf)



a <- tokenize_ngrams(whole_txt,lowercase = TRUE, n_min=3L)
