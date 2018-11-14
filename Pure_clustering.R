#Loading the needed packages
library(tm)
library(proxy)
library(fpc)   
library(wordcloud)
library(cluster)
library(tm)
library(stringi)
library(proxy)
library(wordcloud)
library(stringr)

#Path where the corpus is located 
path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)
ndocs <- length(corpus)

# Ignore extremely rare words that occur only in 2% of the documents
minTermFreq <- ndocs * 0.02

# ignore very common words that occurs in more than 75% of the documents
maxTermFreq <- ndocs * .75

#Convert the corpus into document term matrix
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))


#convert document term matrix to matrix form
dtm.matrix = as.matrix(dtm)

#plot wordcloud by using the top 20 words
wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

#weigh the matrix using tfidf
dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)

#compute the distance matrix 
distMatrix <- dist(dtm, method="euclidean")

# perform k-means clustering with k=5 and store the name of the text and cluster membership results in a data frame
m <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)

#write the cluster membership results in a data frame
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster3.csv")

#read it back again with column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster3.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_3= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_3)= c("text", "cluster")
write.csv(pure_cl_3, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_3.csv")

