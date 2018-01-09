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

ngrams_tokenizer <- function(x)
  unlist(tokenize_ngrams(x,lowercase = TRUE, n=3, n_min=1))
controls <- list(removePunctuation=TRUE, tokenize = ngrams_tokenizer,removeNumbers=TRUE, stemming = TRUE) 

tf <- termFreq(whole_txt, control = controls)
most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 100))
View(most_freq_tf)




corpus <- data.frame()
setwd('~/Documents/DMBD/DMBD/big_data_project_confidential/')
for (file in list.files(pattern="SAT*")) {
  if('Q1bis' %in% colnames(doc1)) next;
  print(file)
  doc <- readLines(file)[-2]
  doc1 <- read.csv(textConnection(doc), header = TRUE, sep = ";")

  if('Q1bis' %in% colnames(doc1)){
    bad <- data.frame(doc1$Q1bis)
  } else {
    mean_satis <- mean(na.omit(doc1[,'Q1']))
    std_satis <- sd(na.omit(doc1[,'Q1']))
    
    note_bad <- mean_satis-std_satis
    bad <- data.frame(doc1[doc1$Q1 < note_bad,]$Q2)
    colnames(bad) <- c('comment')
    bad <- data.frame(bad[bad$comment != '',])
    colnames(bad) <- c('comment')
    bad <- data.frame(bad[!is.na(bad$comment),])
    colnames(bad) <- c('comment')
    bad <- cbind(row.names(bad),bad)
    colnames(bad) <- c('doc_id','text')
  }
 
  corpus <- rbind(corpus,bad)
}  

corpus_bad <- Corpus(DataframeSource(corpus))
tm <- tm_map(corpus_bad, removeWords, stopwords("fr"))
whole_txt = apply(data.frame(tm$content),1,paste,collapse=". ")

ngrams_tokenizer <- function(x)
  unlist(tokenize_ngrams(x,lowercase = TRUE, n=3, n_min=3))
ctrls <- list(removePunctuation=TRUE, tokenize = ngrams_tokenizer,removeNumbers=TRUE, stemming=TRUE, wordLengths=c(3,Inf))

tf <- termFreq(whole_txt, control = ctrls)
most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 100))
View(most_freq_tf)









