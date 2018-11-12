setwd('E:/Roche/NLP/site/')
library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)

site <- readxl::read_excel("site_and_subject.xlsx",col_names = T)
site.description <- unlist(lapply(site$Description, function(x){unlist(strsplit(x,"\n|\r|\n\r"))}))
site.description <- site.description[-grep("^$",site.description)]

site.action <- unlist(lapply(site$`Planned Action`, function(x){unlist(strsplit(x,"\n|\r|\n\r"))}))
site.action <- site.action[-grep("^$",site.action)]

site.resolution <- unlist(lapply(site$Resolution, function(x){unlist(strsplit(x,"\n|\r|\n\r"))}))
site.resolution <- site.action[-grep("^$",site.resolution)]

# write.table(as.matrix(site.resolution),quote = F,row.names = F,col.names = F,file = "Resolution.txt")

sentence_classify <- function(site.description){
  Description.text <- Corpus(VectorSource(site.description))
  Description.DTM <- DocumentTermMatrix(Description.text)
  Description.TDM <- TermDocumentMatrix(Description.text)
  Description.DTM.matrix <- as.matrix(Description.DTM)
  k <- 5
  kmeansRes <- kmeans(Description.DTM.matrix,k)
  mode(kmeansRes)
  kmeansRes$size
  cluster1label <- which(kmeansRes$cluster==1)
  context1 <- c()
  j=1
  for (i in cluster1label) {
    context1[j] <- Description.text[[i]]$content
    j=j+1
  }
  
  cluster2label <- which(kmeansRes$cluster==2)
  context2 <- c()
  j=1
  for (i in cluster2label) {
    context2[j] <- Description.text[[i]]$content
    j=j+1
  }
  
  cluster3label <- which(kmeansRes$cluster==3)
  context3 <- c()
  j=1
  for (i in cluster3label) {
    context3[j] <- Description.text[[i]]$content
    j=j+1
  }
  
  cluster4label <- which(kmeansRes$cluster==4)
  context4 <- c()
  j=1
  for (i in cluster4label) {
    context4[j] <- Description.text[[i]]$content
    j=j+1
  }
  
  cluster5label <- which(kmeansRes$cluster==5)
  context5 <- c()
  j=1
  for (i in cluster5label) {
    context5[j] <- Description.text[[i]]$content
    j=j+1
  }
  
  write.csv(context1,"context1.csv")
  write.csv(context2,"context2.csv")
  write.csv(context3,"context3.csv")
  write.csv(context4,"context4.csv")
  write.csv(context5,"context5.csv")
}
sentence_classify(site.action)


Description.DTM.kmeansRes <- list(content=Description.DTM.matrix,type=kmeansRes$cluster)
write.csv(Description.DTM.kmeansRes,"Description.DTM.kmeansRes.csv")
fix(Description.DTM.kmeansRes)

all.text.norm <- tm_map(all.text,PlainTextDocument)
# Eliminate the space
all.text.norm <- tm_map(all.text,stripWhitespace)
# Remove the Numbers
all.text.norm <- tm_map(all.text.norm,removeNumbers)
# Punctuation removal
all.text.norm <- tm_map(all.text.norm,removePunctuation)
# lowercase
all.text.norm <- tm_map(all.text.norm,tolower)
# remove stopping words
stopwords <- readLines('stopwords.txt')
stopwords <- as.vector(as.character(stopwords))
all.text.norm <- tm_map(all.text.norm, removeWords, stopwords)
# suffix stripping
all.text.norm.root <- tm_map(all.text.norm,stemDocument)
DTM <- DocumentTermMatrix(all.text.norm.root)
DTM.matrix <- as.matrix(DTM)


###hclust
d <- dist(Description.DTM.matrix,method="euclidean")
hclustRes <- hclust(d,method="complete")
hclustRes.type <- cutree(hclustRes,k=5)
length(hclustRes.type)




# Description <- as.vector(as.character(site$Description))
# Planned_Action <- as.vector(as.character(site$`Planned Action`))
# Resolution <- as.vector(as.character(site$Resolution))
# all.text <- Corpus(VectorSource(c(Description,Planned_Action,Resolution)))
# 
# all.text.norm <- tm_map(all.text,PlainTextDocument)
# # Eliminate the space
# all.text.norm <- tm_map(all.text,stripWhitespace)
# # Remove the Numbers
# all.text.norm <- tm_map(all.text.norm,removeNumbers)
# # Punctuation removal
# all.text.norm <- tm_map(all.text.norm,removePunctuation)
# # lowercase
# all.text.norm <- tm_map(all.text.norm,tolower)
# # remove stopping words
# stopwords <- readLines('stopwords.txt')
# stopwords <- as.vector(as.character(stopwords))
# all.text.norm <- tm_map(all.text.norm, removeWords, stopwords)
# # suffix stripping
# all.text.norm.root <- tm_map(all.text.norm,stemDocument)
# DTM <- DocumentTermMatrix(all.text.norm.root)
# DTM.matrix <- as.matrix(DTM)
# 
# k <- 5
# kmeansRes <- kmeans(DTM.matrix,k) 
# mode(kmeansRes)
# kmeansRes$size
# hlzj.kmeansRes <- list(content=DTM.matrix,type=kmeansRes$cluster)
# # write.csv(hlzj.kmeansRes,"hlzj_kmeansRes.csv")
# fix(hlzj.kmeansRes)
# 
# 
# TDM <- TermDocumentMatrix(all.text.norm.root)
# terms <- findFreqTerms(TDM)
# term.freq.matrix <- TDM[terms,] %>% as.matrix() %>% data.frame() %>% arrange(desc(rowSums(.)))
# term.freq.sorted <- TDM[terms,] %>% as.matrix() %>% rowSums() %>% data.frame(Term = terms, Freq = .) %>% arrange(desc(Freq)) %>% cbind(. ,term.freq.matrix)
# 
# library(wordcloud2)
# data.freq <- data.frame(Term = term.freq.sorted$Term,freq = log(term.freq.sorted$Freq))
# wordcloud2(data.freq,size = 0.5,shape = "star")
