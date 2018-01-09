if (FALSE) {
  install.packages('tm')
  install.packages('tokenizers')
}

library(tm)
library(tokenizers)

rm(list = ls(all = TRUE))

corpus <- data.frame()
setwd('~/Documents/DMBD/DMBD/big_data_project_confidential/')
for (file in list.files(pattern="SAT*")) {
  # print(file)
  doc <- readLines(file)[-2]
  doc1 <- read.csv(textConnection(doc), header = TRUE, sep = ";")

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



corpus_bad <- Corpus(DataframeSource(corpus))
skipWordsfr <- function(x) removeWords(x, stopwords("fr"))
skipWords <- function(x) removeWords(x, c("j'"))
funs <- list(skipWords,
             skipWordsfr)
tm <-tm_map(corpus_bad, FUN = tm_reduce, tmFuns = funs)

whole_txt = apply(data.frame(tm$content),1,paste,collapse=". ")

ngrams_tokenizer <- function(x) {
  unlist(tokenize_ngrams(x,lowercase = TRUE, n=3, n_min=3))
}
ctrls <- list(removePunctuation=TRUE, tokenize = ngrams_tokenizer,removeNumbers=TRUE, stemming=TRUE, wordLengths=c(3,Inf))

tf <- termFreq(whole_txt, control = ctrls)
most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 100))
View(most_freq_tf)









