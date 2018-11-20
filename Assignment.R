setwd("D:/Roche/")

assignmentFUN <- function(x,num=10){
  library(XML)
  library(tm)
  library(SnowballC)
  library(hashmap)
  library(dplyr)
  library(ggplot2)
  collection <- readLines(x)
  parsed_collection <- htmlParse(collection)
  # extract text
  text_parsed_collection <- getNodeSet(parsed_collection, '//text')
  all.text <- sapply(text_parsed_collection, xmlValue)
  # build Corpus
  all.text <- Corpus(VectorSource(all.text))
  # Eliminate the space
  all.text.norm <- tm_map(all.text,stripWhitespace)
  # Remove the Numbers
  all.text.norm <- tm_map(all.text.norm,removeNumbers)
  # Punctuation removal
  all.text.norm <- tm_map(all.text.norm,removePunctuation)
  # lowercase
  all.text.norm <- tm_map(all.text.norm,tolower)
  # suffix stripping
  all.text.norm.root <- tm_map(all.text.norm,stemDocument)
  # inspect(all.text.norm.root)
  # m <- DocumentTermMatrix(all.text.norm.root)
  TDM <- TermDocumentMatrix(all.text.norm.root)
  terms <- findFreqTerms(TDM)
  term.freq.matrix <- TDM[terms,] %>% as.matrix() %>% data.frame() %>% arrange(desc(rowSums(.)))
  term.freq.sorted <- TDM[terms,] %>% as.matrix() %>% rowSums() %>% data.frame(Term = terms, Freq = .) %>% arrange(desc(Freq)) %>% cbind(. ,term.freq.matrix)
  show <- apply(term.freq.sorted,1,function(x){y=paste("[",c(x[1],paste(c("1",x[3]),collapse = ",",sep = ""),
                                                             paste(c("2",x[4]),collapse = ",",sep = ""),
                                                             paste(c("3",x[5]),collapse = ",",sep = "")),"]",sep = "");paste(y,collapse = " -> ")})
  print(show[1:num])
  
  histogram.data <- data.frame(t(table(term.freq.sorted$Freq)))
  p <- ggplot(histogram.data, aes(Var2, Freq)) + 
    geom_bar(stat="identity", position="dodge")+
    geom_text(aes(x=Var2,y=Freq+Freq/4,label=Freq))+
    scale_y_continuous(trans="log2")+
    labs(x="Count",y="Frequency")+
    theme(axis.title.x=element_text(size = 15),
          axis.title.y=element_text(size = 15))
  print(p)
  return(term.freq.sorted)
}

generatehash <- function(x){
  value <- c()
  for(i in 1:nrow(x)){
    hash.value <- c()
    k=1
    for(j in 3:5){
      if(x[i,j] != 0){
        hash.value[k] <- paste(j-2,x[i,j],sep = ",")
        k=k+1
      }
    }
    value[i] <- paste(hash.value,collapse = "->")
  }
  term.Freq.hash <- hashmap(as.character(x[,1]),value)
  return(term.Freq.hash)
}

result <- assignmentFUN("collection.txt",num = 15)
freq.hash <- generatehash(result)


library(wordcloud2)
data.freq <- data.frame(Term = result$Term,freq = log(result$Freq))
wordcloud2(data.freq,size = 0.5,shape = "circle")
# remove useless information and generate tree structure
# h2 <- list(
#   startElement = function(node) {
#     name = xmlName(node)
#     if(name %in% c("date","section","type","length","headline","byline")) {NULL}
#     else {node}
#   }, 
#   comment = function(node) {NULL}
# )
# parsed_collection_tree <- htmlTreeParse(collection, handlers = h2, asTree = TRUE)


