library(tm)
library(MASS)
library(ggplot2)
library(fitdistrplus)
library(poweRlaw)
library(stringi)
library(compiler)
library(stringr)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(lsa)

setwd("~/Desktop/Complexity1/Project")
conn<-file("child4.txt",open="r")
d<-readLines(conn)
close(conn)
singleString <- paste(d,collapse="\n")
Z <- str_split(singleString,pattern="\\\n(\\s)?\\\n")[[1]]
docs<-Corpus(VectorSource(Z))

#docs <- tm_map(docs, function(x) stri_replace_all_regex(x, "[[:punct:]]", " "))
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c(stopwords("english"),"th","s"))
docs <- tm_map(docs, stripWhitespace)
docs1<-docs
docs <- tm_map(docs, stemDocument,language="english")
docs <- tm_map(docs, PlainTextDocument)
for(i in 1:length(docs)){
  docs[[i]]$meta<-NULL
}
docs$meta<-NULL
writeLines(as.character(docs), con="stemCorpusChild3.txt")
writeLines(as.character(docs1), con="corpusChild3.txt")

conn<-file("stemCorpusChild1.txt",open="r")
d<-readLines(conn)
close(conn)
docs<-Corpus(VectorSource(d))

Tdm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
x<-t(Tdm)
#Number of documents
numDocs<-nrow(x)
#Number of terms
numTerms<-ncol(x)
#Number of words in each document
nOW<-as.vector(slam::row_sums(x))

mle<-fitdistr(nOW[nOW>0], "lognormal")
meanlogemp <- mle$estimate["meanlog"]
sdlogemp <- mle$estimate["sdlog"]

ks.test(nOW[nOW>0], plnorm, meanlog = meanlogemp, sdlog = sdlogemp) 


maxDoc<-max(nOW)
mydata_hist<-hist(nOW,probability = TRUE, breaks = maxDoc, col = "darkslategray4", border = "seashell3")

z<-seq(0,1500,1)

#Calculates the probability that a document has certain number of words, using data from the histogram produced above.
prob<-mydata_hist$count/numDocs
#Plot empirical vs fitted distribution density functions
plot(mydata_hist$mid,log="xy",prob,lwd=1, lend=1)
lines(dlnorm(z,meanlog = meanlogemp, sdlog = sdlogemp),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "firebrick2", lwd = 3)

result<-analyseCorpus(docs)

#Vector with the document sizes
Ls<-ceiling(rlnorm(numDocs,meanlog = meanlogemp, sdlog = sdlogemp))
#Initial ranking
r0<-seq(1,numTerms,1)
#Creating the artificial corpus
cc<-lapply(Ls,createDocument,numTerms=numTerms,r0=r0)
docs <- Corpus(VectorSource(unlist(cc)))
#Saving it into a file
writeLines(as.character(docs), con="artificialCorpus1Child1.txt")


conn<-file("artificialCorpus1.txt",open="r")
d<-readLines(conn)
close(conn)
docs1<-Corpus(VectorSource(d))

#Similarity

Tdmatrix <- as.matrix(Tdm)

a1<-proc.time()
dist.mat <- cosine(Tdmatrix)
a2<-proc.time()
a2-a1
dist.vec<-as.dist(dist.mat)
dist.vec<-dist.vec[dist.vec!=0]
mydist_hist<-hist(dist.vec,probability = TRUE, breaks = 1000, col = "darkslategray4", border = "seashell3")
