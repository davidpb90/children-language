setwd("~/Desktop/Complexity1/Project/sector")

library(tm)
library(stringi)
library(proxy)
library(XML)
library(SnowballC)
library(lsa)

#library(boilerpipeR)
#path=".";
#html <- list.files(recursive=TRUE)#pattern="\\.(htm|html)$")

#Creates list of all url's in the dataset
html1<-list.files(recursive=TRUE,pattern="\\.(htm|html)$")

#while(length(html1)==0){
#  path<-stri_paste(path,"/",html[1])
#  html<-list.files(path)
#  html1<-list.files(path,pattern="\\.(htm|html)$")
#} 
#articles <- character(length(html1))
#flatten.text <- function(text){
 # text<-stri_flatten(c(text,linn[j]),col = " ")
#}

#Function to create a corpus with the document in url.name
create.corpus <- function(url.name){
  text<-""
  conn<-file(url.name,open="r")
  linn<-readLines(conn,encoding = "utf-8")
  linn <- iconv(linn, to = "utf-8", sub=" ")
  for (j in 10:length(linn)){
    text<-stri_flatten(c(text,linn[j]),col = " ")
  }
  close(conn)
  cc=Corpus(VectorSource(text))
  meta(cc,"link")=url.name
  return(cc)
}

#link=catch$url

#Create a corpora where each document is a corpus
cc <- lapply(html1, create.corpus)
#Create only one corpus out of all the documents
docs <- Corpus(VectorSource(unlist(cc)))
meta(docs,'link') = do.call(rbind,lapply(cc,meta))$link


#do.call(function(...) c(..., recursive = TRUE), cc)
#for (i in 1:length(html1)) {
  
#  conn<-file(html1[i],open="r")
  
#  linn<-readLines(conn)
  
#  for (j in 10:length(linn)){
#    articles[i]<-stri_flatten(c(articles[i],linn[j]),col = " ")
#  }
#  close(conn)
  #articles[i] <- readLines(stri_paste(main, titles[i]))
  #articles[i]<-articles[i][8:length(articles[i])]
  #articles[i]<-stri_flatten(articles[i],col = " ")
#}

#docs <- Corpus(VectorSource(articles))

#Removes html language
docs <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
#Removes all \t
docs <- tm_map(docs, function(x) stri_replace_all_fixed(x, "\t", " "))

docs <- tm_map(docs, function(x) stri_replace_all_regex(x, "[[:punct:]]", " "))
docs <- tm_map(docs, function(x) stri_replace_all_regex(x, "[^[:alnum:]]", " "))
#Transforms the document into plain text format
docs <- tm_map(docs, PlainTextDocument)
#Removes english stopwords and "nbsp"
docs <- tm_map(docs, removeWords, c(stopwords("english"),"nbsp"))
#Changes punctuation for a space
#changePunctuation<-function(x) gsub("[[:punct:]]", " ", x)
#Removing the / is not working
#docs <- tm_map(docs, function(x) stri_replace_all_fixed(x, "/", " "))
#Removes punctuation
docs <- tm_map(docs, removePunctuation)
#Trasnforms all capital letters into lower case
docs <- tm_map(docs, tolower)
#Removes numbers
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, c(stopwords("english"),"th","s"))
#Function to remove all url's


#Removes url's
docs <- tm_map(docs, function(x) gsub("http[[:alnum:]]*", " ", x))
#Removes extra whitespaces
docs <- tm_map(docs, stripWhitespace)

#Stemming

# make sure that packages below have been installed
# Otherwise, stem completion will fail.
#Libraries for stemming

library(RWeka)
library(rJava)
library(RWekajars)
# keep a copy of corpus to use later as a dictionary for stem completion
 docsCopy <- docs
# stem words
docs <- tm_map(docs, stemDocument,language="english")
docsCopy2<-docs

docs <- tm_map(docs, PlainTextDocument)
#docs <- tm_map(docs, stemCompletion, dictionary=docsCopy)
#Recover the corpus from memory
setwd("~/Desktop/Complexity1/Project")
#writeLines(as.character(docs), con="stemCorpus.txt")
#writeLines(as.character(docsCopy), con="originalCorpus.txt")
conn<-file("stemCorpus.txt",open="r")
d<-readLines(conn)
close(conn)
docs<-Corpus(VectorSource(d))
#Important!! The transformations change the class of docs, thus you have to change it back 
#to a VectorSource.
docs<-Corpus(VectorSource(docs))
docsCopy<-Corpus(VectorSource(docsCopy))
#Create matrix
Tdm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
TdmOriginal<-TermDocumentMatrix(docsCopy, control=list(wordLengths=c(1,Inf)))
Tdm<-t(Tdm)
y <- log(sort(slam::col_sums(Tdm), decreasing = TRUE))
x <- log(seq_along(y))

plot(x,y)
#idx <- which(dimnames(Tdm)$Terms == "r")
#idxOriginal<-which(dimnames(TdmOriginal)$Terms == "r")
#inspect(TdmOriginal[idx+(0:10),101:3000])
#Zipf_plot(Tdm)
Zipf_plot_mod(Tdm)
Heaps_plot(Tdm)

analyseCorpus<-function(docs){
  Tdm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
  par(mfrow=c(2,1))
  a<-Zipf_plot_mod(Tdm)
  b<-Heaps_plot_mod(Tdm)
  return(list(a,b))
}

result<-analyseCorpus(docs)

##Comparing corpora
Tdm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
Tdm1<-TermDocumentMatrix(docs1, control=list(wordLengths=c(1,Inf)))
comparisonZipf(Tdm,Tdm1)
comparisonHeap(Tdm,Tdm1)
comparisonZipf<-function(Tdm,Tdm1){
  
  x<-t(Tdm)
  x1<-t(Tdm1)
  y <- log(sort(slam::col_sums(x), decreasing = TRUE))
  x <- log(seq_along(y))
  y1 <- log(sort(slam::col_sums(x1), decreasing = TRUE))
  x1 <- log(seq_along(y1))
  plot(x,y,col="blue",xlab="log(rank)",ylab="log(frequency)",main="Zipf's law")
  points(x1,y1,col="red")
  legend("topright", c("Original corpus","Simulated corpus"),lty=c(1,1),col=c("blue","red"))
  
}

comparisonZipfAll<-function(Tdm,Tdm1,Tdm2,Tdm3,Tdm4){
  
  x<-t(Tdm)
  x1<-t(Tdm1)
  x2<-t(Tdm2)
  x3<-t(Tdm3)
  x4<-t(Tdm4)
  
  y <- log(sort(slam::col_sums(x), decreasing = TRUE))
  x <- log(seq_along(y))
  y1 <- log(sort(slam::col_sums(x1), decreasing = TRUE))
  x1 <- log(seq_along(y1))
  y2 <- log(sort(slam::col_sums(x2), decreasing = TRUE))
  x2 <- log(seq_along(y2))
  y3 <- log(sort(slam::col_sums(x3), decreasing = TRUE))
  x3 <- log(seq_along(y3))
  y4 <- log(sort(slam::col_sums(x4), decreasing = TRUE))
  x4 <- log(seq_along(y4))
  
  plot(x,y,col="blue",xlab="log(rank)",ylab="log(frequency)",main="Zipf's law")
  points(x1,y1,col="red")
  points(x2,y2,col="green")
  points(x3,y3,col="yellow")
  points(x4,y4,col="black")
  legend("topright", c("IS corpus","Child1","Child2","Child3","Child4"),
         lty=c(1,1),col=c("blue","red","green","yellow","black"))
  
}

comparisonHeapAll<-function(Tdm,Tdm1,Tdm2,Tdm3,Tdm4){
  
  x<-t(Tdm)
  x1<-t(Tdm1)
  x2<-t(Tdm2)
  x3<-t(Tdm3)
  x4<-t(Tdm4)
  y <- log(cum_vocabulary_size(x))
  x <- log(cumsum(slam::row_sums(x)))
  y1 <- log(cum_vocabulary_size(x1))
  x1 <- log(cumsum(slam::row_sums(x1)))
  y2 <- log(cum_vocabulary_size(x2))
  x2 <- log(cumsum(slam::row_sums(x2)))
  y3 <- log(cum_vocabulary_size(x3))
  x3 <- log(cumsum(slam::row_sums(x3)))
  y4 <- log(cum_vocabulary_size(x4))
  x4 <- log(cumsum(slam::row_sums(x4)))
  plot(x,y,col="blue",xlab="log(T)",ylab="log(V)",main="Heap's law")
  points(x1,y1,col="red")
  points(x2,y2,col="green")
  points(x3,y3,col="yellow")
  points(x4,y4,col="black")
  legend("topright", c("IS corpus","Child1","Child2","Child3","Child4"),
         lty=c(1,1),col=c("blue","red","green","yellow","black"))
}

##Similarity

Tdmatrix <- as.matrix(Tdm)

a1<-proc.time()
dist.mat <- cosine(Tdmatrix)
a2<-proc.time()
a2-a1
dist.vec<-as.dist(dist.mat)
dist.vec<-dist.vec[dist.vec!=0]
mydist_hist<-hist(dist.vec,probability = TRUE, breaks = 1000, col = "darkslategray4", border = "seashell3")


#A way to reduce the code???
#corp = Corpus(VectorSource(text)) 

#parameters = list(minDocFreq        = 1, 
#                  wordLengths       = c(2,Inf), 
#                  tolower           = TRUE, 
#                  stripWhitespace   = TRUE, 
#                  removeNumbers     = TRUE, 
#                  removePunctuation = TRUE, 
#                  stemming          = TRUE, 
#                  stopwords         = TRUE, 
#                  tokenize          = NULL, 
#                  weighting         = function(x) weightSMART(x,spec="ltn"))

#tdm = TermDocumentMatrix(corp,control=parameters)

Zipf_plot_mod<-function (x, type = "l", ...) 
{
  if (inherits(x, "TermDocumentMatrix")) 
    x <- t(x)
  y <- log(sort(slam::col_sums(x), decreasing = TRUE))
  x <- log(seq_along(y))
  m <- lm(y ~ x)
  dots <- list(...)
  if (is.null(dots$xlab)) 
    dots$xlab <- "log(rank)"
  if (is.null(dots$ylab)) 
    dots$ylab <- "log(frequency)"
  plot(x,y)
  #do.call(plot, c(list(x, y, type = type), dots))
  abline(m)
  coef(m)
  
}

Heaps_plot_mod<-function (x, type = "l", ...) 
{
  if (inherits(x, "TermDocumentMatrix")) 
    x <- t(x)
  y <- log(cum_vocabulary_size(x))
  x <- log(cumsum(slam::row_sums(x)))
  m <- lm(y ~ x)
  dots <- list(...)
  if (is.null(dots$xlab)) 
    dots$xlab <- "log(T)"
  if (is.null(dots$ylab)) 
    dots$ylab <- "log(V)"
  #do.call(plot, c(list(x, y, type = type), dots))
  plot(x,y)
  abline(m)
  coef(m)
}

cum_vocabulary_size<-function (m) 
{
  ###This line gives the number of the document in which each word appears for the first time.
  i <- sapply(split(m$i, m$j), min)
  ###This creates a table of the number of new words in each document, counting the number of times 
  # each document appears in the table above. 
  tab <- table(i)
  
  v <- double(nrow(m))
  v[as.numeric(names(tab))] <- tab
  #It creates the cumulative sums of words in the documents.
  cumsum(v)
}