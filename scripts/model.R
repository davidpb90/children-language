library(tm)
library(MASS)
library(ggplot2)
library(fitdistrplus)
library(poweRlaw)
library(stringi)
library(compiler)
library(sos)
library(proxy)
setwd("~/Desktop/Complexity1/Project")
conn<-file("child1.txt",open="r")
d<-readLines(conn)
close(conn)

docs<-Corpus(VectorSource(d))

Tdm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
x<-t(Tdm)

numDocs<-nrow(x)
numTerms<-ncol(x)
nOW<-as.vector(slam::row_sums(x))
totalNOW<-sum(nOW)
maxDoc<-max(nOW)
#numberOfWords<-numberOfWords[numberOfWords>8]
mydata_hist<-hist(nOW,probability = TRUE, breaks = maxDoc, col = "darkslategray4", border = "seashell3")
lines(density(nOW - 0.5),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "firebrick2", lwd = 3)
#Maximum likelihood estimation for the lognormal
mle<-fitdistr(nOW[nOW>8], "lognormal")
meanlogemp <- mle$estimate["meanlog"]
sdlogemp <- mle$estimate["sdlog"]

#Kolmogorov-Smirnov test
ks.test(nOW[nOW>8], plnorm, meanlog = meanlogemp, sdlog = sdlogemp) 

z<-seq(0,1500,1)
#plot(z,dlnorm(z,meanlog = meanlogemp, sdlog = sdlogemp),log="xy",col="red", main="ECDF and
#Weibull CDF")

#fitted<-fitdist(nOW, "lnorm")
#plot(fitted,log="xy")
#ggplot(mydata_hist, aes(x = V3)) + geom_histogram() + scale_x_log10()

#Calculates the probability that a document has certain number of words
prob<-mydata_hist$count/numDocs
#Plot empirical vs fitted distribution density functions
plot(mydata_hist$mid,log="xy",prob,lwd=1, lend=1)
lines(dlnorm(z,meanlog = meanlogemp, sdlog = sdlogemp),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "firebrick2", lwd = 3)
#plot(density(prob),log='xy')
#plot(function(x) {dlnorm(x, meanlog = meanlogemp, sdlog = sdlogemp)},1, 15000,ylab="lognormal density")




##
#Nice plots
##

#fit <- fitdistr(data$x, "lognormal")
#ggplot(data=mydata_hist$counts/ncol(x), aes(x=x)) +
#  geom_histogram(aes(y = ..density..)) +
#  stat_function(fun = dlnorm, size = 1, color = 'gray',
#               args = list(mean = fit$estimate[1], sd = fit$estimate[2])) +  theme_bw() 

####Model#####
 
#C function
dyn.load("reorder.so")
reorder <- function(df,r0,ind) {
  if (!is.numeric(df) || !is.numeric(r0) || !is.numeric(ind))
    stop("arguments must be numeric")
  out <- .C("reorder",
            df=as.integer(df), as.integer(r0), as.integer(ind), as.integer(length(r0)))
        
  return(out$df)
}


createDocument<-function(L, numTerms,r0){
  counts<-rep(0,numTerms)
  r<-r0
  text<-rep(0,L)
  df<-cbind(r,counts)
  
  p<-1/seq(1,numTerms,1)
  #Normalization constant
  C<-1/sum(p)
  #probability density function
  pd<-C*p
  #ecdf
  ecdf=cumsum(pd)
  probs <- replicate(L,length(ecdf[ecdf<runif(1)])+1)
  i=1
  ord<-rep(0,numTerms)
  while(i<=L){
    #It orders first according to the counts and then to the original ranking r0
    ind=probs[i]
    #Solucion de una linea
    #df<-df[order(-df[,2],match(df[,1],r0)),]
    #text[i]<-df[,1][probs[i]]
    #df[,2][probs[i]]<-df[,2][probs[i]]+1
    #Solucion sin modificar la matriz df
    ord<-order(-df[,2],df[,1])
    text[i]<-df[ord[ind],1]
    df[ord[ind],2]<-df[ord[ind],2]
    #text[i]<-df[ind,1]
    #df[ind,2]<-df[ind,2]+1
    #df<-reorder(df,r0,ind)
    #df<-cbind(df[1:numTerms],df[(numTerms+1):(2*numTerms)])
    
    #j=ind-1
    #temp=df[ind,]
    #repeat{
    #  if(j==0 || df[ind,2]<df[j,2] || (df[ind,2]==df[j,2] && match(df[ind,1],r0)>match(df[j,1],r0))){
    #    if(j==ind-1){
    #      break
    #    }
    #    else{
    #      df[(j+2):ind,]<-df[(j+1):(ind-1),]
    #      df[j+1,]<-temp
    #      break
    #    }
    #    
    #  }
    #  else{
    #    j<-j-1
    #  }
    #}
    i<-i+1
  }
  #ord<-order(-df[,2],df[,3])
  text<-paste(text,collapse=" ")
  cc<-Corpus(VectorSource(text))
  #newList <- list(text, df)
  return(cc)
  
}
#Vector with the document sizes
Ls<-ceiling(rlnorm(numDocs,meanlog = meanlogemp, sdlog = sdlogemp))
#Creating the artificial corpus
cc<-lapply(Ls,createDocument,numTerms=numTerms,r0=a)
docs <- Corpus(VectorSource(unlist(cc)))
#Saving it into a file
writeLines(as.character(docs), con="artificialCorpus1.txt")
#Reading it from memory
conn<-file("artificialCorpus1.txt",open="r")
d<-readLines(conn)
close(conn)
docs1<-Corpus(VectorSource(d))

a<-seq(1,numTerms,1)
Ls<-ceiling(rlnorm(numDocs,meanlog = meanlogemp, sdlog = sdlogemp))
createCorpusComp <- cmpfun(createCorpus)
a1<-proc.time()
cc<-lapply(Ls,createDocument,numTerms=numTerms,r0=a)
a2<-proc.time()
a2-a1
docs <- Corpus(VectorSource(unlist(cc)))
writeLines(as.character(docs), con="artificialCorpus1.txt")
conn<-file("artificialCorpus1.txt",open="r")
d<-readLines(conn)
close(conn)
docs1<-Corpus(VectorSource(d))



b1<-proc.time()
cc<-lapply(Ls[1:2],createCorpusComp,numTerms=numTerms,r0=a)
b2<-proc.time()

b2-b1
###Profiling
Rprof("out.out")
cc<-lapply(Ls[1:10],createCorpus,numTerms=numTerms,r0=a)
Rprof(NULL)
proftable("out.out")
###


##Proving the sorting
#The position in r corresponds to the ranking of the word placed there.
a<-seq(1,10,1)
r0<-sample(a)
c<-sample(seq(1,5,1),size=10,replace=TRUE)
r<-r0
a<-r
df<-data.frame(a,r,c,r0)
df[order(-c,r0),]
#Normalization constant
C<-1/sum(1/seq(1,ncol(x),1))
#probability density function
pd<-C*1/seq(1,ncol(x),1)
#ecdf
ecdf=cumsum(pd)
probs=replicate(10,length(ecdf[ecdf<runif(1)])+1)

lista<-createCorpus(16,10,a)

lista2<-rep(list(lista), 2)

lista3<-data.frame(Ls,rep(numTerms,numDocs),rep(list(a),numDocs))




