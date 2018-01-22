if (FALSE) {
  install.packages('tm')
  install.packages('tokenizers')
}

library(tm)
library(tokenizers)
library(xtable)

rm(list = ls(all = TRUE))

setwd('~/Documents/DMBD/DMBD/big_data_project_confidential/')
# setwd(dir = "/home/mathieu/Documents/Master/M2/S3/BigData/Projet/big_data_project_confidential/")

for (op in c('sup','inf')) {
  corpus <- data.frame()
  for (file in list.files(pattern="SAT*")) {
    doc <- readLines(file)[-2]
    doc1 <- read.csv(textConnection(doc), header = TRUE, sep = ";")
    na.omit(doc1)
    if('Q1bis' %in% colnames(doc1)){
      if (op=='inf') {
        a <- data.frame(doc1$Q1bis)
        b <- data.frame(doc1$Q3)
        c <- data.frame(doc1$Q4)
        colnames(a) <- c('comment')
        colnames(b) <- c('comment')
        colnames(c) <- c('comment')
        sub <- rbind(a,b,c)
      }
      else next()
    } else {
      mean_satis <- mean(na.omit(doc1[,'Q1']))
      std_satis <- sd(na.omit(doc1[,'Q1']))
      
      note_bad <- mean_satis-std_satis
      if (op == 'inf') {
        sub <- data.frame(doc1[doc1$Q1 < note_bad,]$Q2)
      } else {
        sub <- data.frame(doc1[doc1$Q1 > note_bad,]$Q2)
      }
    }
    colnames(sub) <- c('comment')
    sub <- data.frame(sub[sub$comment != '',])
    colnames(sub) <- c('comment')
    sub <- data.frame(sub[!is.na(sub$comment),])
    colnames(sub) <- c('comment')
    sub <- cbind(row.names(sub),sub)
    colnames(sub) <- c('doc_id','text')
  
    corpus <- rbind(corpus,sub)
  }  
  
  
  shorts <- c('j','c','a','y','à','d','n','ere','m','l')
  corpus_bad <- Corpus(DataframeSource(corpus))
  corpus_bad <- tm_map(corpus_bad, content_transformer(tolower))
  corpus_bad <- tm_map(corpus_bad, removeNumbers)
  corpus_bad <- tm_map(corpus_bad, removeWords, c(stopwords('fr'),shorts))
  corpus_bad <- tm_map(corpus_bad, removePunctuation)
  corpus_bad <- tm_map(corpus_bad, stemDocument, language="fr")
  
  tm <- data.frame(corpus_bad$content)
  whole_txt = apply(tm,1,paste,collapse=". ")
  
  ngrams_tokenizer <- function(x) {
    unlist(tokenize_ngrams(x,lowercase = TRUE, n=ngram, n_min=ngram))
  }
  
  for (ngram in c(1,2,3,4,5)){
    ctrls <- list(tokenize = ngrams_tokenizer, stemming=FALSE, wordLengths=c(ngram*2, ngram*10))
    
    tf <- termFreq(whole_txt, control = ctrls)
    most_freq_tf <- data.frame(findMostFreqTerms(tf, n = 100))
    filename = paste('../results/csv/term-freq-',op,'-',ngram,'.csv',sep='')
    write.csv(x=most_freq_tf,file = filename)
    # sink(file = filename)
    # xtable(most_freq_tf)
    # sink()
  }
}



########################################
# SEARCHING FOR INTERSECTION IN TABLES #
########################################
for (ngram in c(1,2,3,4,5)) {
  inf <- read.csv(paste('../results/csv/term-freq-inf-',ngram,'.csv',sep=''))
  sup <- read.csv(paste('../results/csv/term-freq-sup-',ngram,'.csv',sep=''))
  colnames(inf) <- c('ngrams','nappear')
  colnames(sup) <- c('ngrams','nappear')
  
  
  # computing the intersection between inf & sup
  inter <- data.frame(intersect(inf[,1],sup[,1]))
  for (ng in inter[,1]) {
    # print(ng)
    inf <- inf[inf$ngrams != ng,]
    sup <- sup[sup$ngrams != ng,]
  }
  
  row.names(inf) <- 1:dim(inf)[1]
  row.names(sup) <- 1:dim(sup)[1]
  
  write.csv(x = inf, file = paste('../results/csv/term-freq-inf-',ngram,'-filtered.csv',sep=''))
  write.csv(x = sup, file = paste('../results/csv/term-freq-sup-',ngram,'-filtered.csv',sep=''))
  
  # generating final latex tables
  if (TRUE) {
    sink(file = paste('../results/tex/term-freq-inf-',ngram,'-filtered.tex',sep=''))
    print(xtable(
      x = inf[0:20,1:2],
      caption = paste('Unsatistied: ',ngram,'-grams term-frequencies',sep=''),
      display = c('d','s','d'),
      label = paste('tab:tf_inf_',ngram,sep='')
      ))
    sink()
  }
  
  # generating final latex tables
  if (TRUE) {
    sink(file = paste('../results/tex/term-freq-sup-',ngram,'-filtered.tex',sep=''))
    print(xtable(
      x = sup[0:20,1:2],
      caption = paste('Satistied: ',ngram,'-grams term-frequencies',sep=''),
      display = c('d','s','d'),
      label = paste('tab:tf_sup_',ngram,sep='')
    ))
    sink()
  }
}











