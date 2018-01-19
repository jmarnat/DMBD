if (FALSE) {
  install.packages('tm')
  install.packages('tokenizers')
}

library(tm)
library(tokenizers)

rm(list = ls(all = TRUE))

corpus <- data.frame()
# setwd('~/Documents/DMBD/DMBD/big_data_project_confidential/')
setwd(dir = "/home/mathieu/Documents/Master/M2/S3/BigData/Projet/big_data_project_confidential/")

for (file in list.files(pattern="SAT*")) {
  # print(file)
  doc <- readLines(file)[-2]
  doc1 <- read.csv(textConnection(doc), header = TRUE, sep = ";")
  na.omit(doc1)
  if('Q1bis' %in% colnames(doc1)){
    a <- data.frame(doc1$Q1bis)
    b <- data.frame(doc1$Q3)
    c <- data.frame(doc1$Q4)
    colnames(a) <- c('comment')
    colnames(b) <- c('comment')
    colnames(c) <- c('comment')
    bad <- rbind(a,b,c)
  } else {
    mean_satis <- mean(na.omit(doc1[,'Q1']))
    std_satis <- sd(na.omit(doc1[,'Q1']))
    
    note_bad <- mean_satis-std_satis
    bad <- data.frame(doc1[doc1$Q1 < note_bad,]$Q2)
  }
  colnames(bad) <- c('comment')
  bad <- data.frame(bad[bad$comment != '',])
  colnames(bad) <- c('comment')
  bad <- data.frame(bad[!is.na(bad$comment),])
  colnames(bad) <- c('comment')
  bad <- cbind(row.names(bad),bad)
  colnames(bad) <- c('doc_id','text')

  corpus <- rbind(corpus,bad)
}  


shorts <- c('j','c','a','y','à','d','n','ere','m','l')
corpus_bad <- Corpus(DataframeSource(corpus))
corpus_bad <- tm_map(corpus_bad, content_transformer(tolower))
corpus_bad <- tm_map(corpus_bad, removeNumbers)
corpus_bad <- tm_map(corpus_bad, removeWords, c(stopwords('fr'),shorts))
corpus_bad <- tm_map(corpus_bad, removePunctuation)
corpus_bad <- tm_map(corpus_bad, stemDocument, language="fr")

tm <- data.frame(corpus_bad$content)
# View(tm)

whole_txt = apply(tm,1,paste,collapse=". ")

n_for_grams <- 5
ngrams_tokenizer <- function(x) {
  unlist(tokenize_ngrams(x,lowercase = TRUE, n=6, n_min=3))
}
ctrls <- list(tokenize = ngrams_tokenizer, stemming=FALSE, wordLengths=c(n_for_grams*2, n_for_grams*10))

tf <- termFreq(whole_txt, control = ctrls)
most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 100))
View(most_freq_tf)






##########################################################
















