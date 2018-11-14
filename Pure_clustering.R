#######################################Cluster 6 

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

path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)

ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less than 1% of the documents
minTermFreq <- ndocs * 0.0
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * 1
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))




#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")





m <- kmeans(distMatrix, 3)
cls=as.data.frame(cl$cluster)



#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster3.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster3.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_3= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_3)= c("text", "cluster")
write.csv(pure_cl_3, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_3.csv")



library(foreign)

co <- read.dta("http://data.princeton.edu/eco572/datasets/cofertx.dta")


#######################################Cluster 4
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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))

#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))



write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)

inspect(dtm)

dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")



cl <- kmeans(distMatrix, 4)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster4.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster4.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_4= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_4)= c("text", "cluster")
write.csv(pure_cl_4, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_4.csv")

######################



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

path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)




ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))

#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


#########################
cl <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "pure_cluster5.csv")

#read it back again with the column name text and cluster
df= read.csv("pure_cluster5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_5.csv")

#######################################Cluster 6 

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

path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))

#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")



cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_6.csv")


#########################################


#######################################Cluster 6 

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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))

#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")



cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_7.csv")


#########################################

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_8.csv")


#################################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_9.csv")


##############################

wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")
############################


############################################################ WO annnexes


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

path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))

#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)




#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes/clustering_8.csv")

#############################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)




#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes/clustering_9.csv")

#############################

cl <- kmeans(distMatrix, 10)
cls=as.data.frame(cl$cluster)




#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_10.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_10.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_10= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_10)= c("text", "cluster")
write.csv(pure_cl_10, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes/clustering_10.csv")





######################


cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes/clustering_7.csv")

#################################
cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes/clustering_6.csv")

#################################
cl <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_5.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes/clustering_5.csv")

#################################

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

path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))

#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")



cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering/clustering_6.csv")


#########################################




#################################################WO ANNEXES BUT NO TFIDF

####################################### STEMMMMEMMMMMMMMMMMMMMMMMMMMMMMED

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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.0
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * 1
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 20),
                           removePunctuation = T,
                           removeNumbers = T,
                           removeWords=c("efta","liechenstein", "turkey","iceland", "efta","chapter", "annexes", "annex", "protocol","protocols")
                           ,stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))



#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


wordcloud(colnames(dtm.matrix), dtm.matrix[1, ], max.words = 150)



wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")
#########################

cl <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_5.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed/clustering_5.csv")

#################################
cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed/clustering_6.csv")

#################################

cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed/clustering_7.csv")

#################################

#################################

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed/clustering_8.csv")

#################################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed/clustering_9.csv")

#################################


cl <- kmeans(distMatrix, 10)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_10.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_10.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_10= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_10)= c("text", "cluster")
write.csv(pure_cl_10, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed/clustering_10.csv")

#################################





#######################################STEMMED 3 w/o annexes - it is close to 1 

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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.0
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * 1
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 20),
                           removePunctuation = T,
                           removeNumbers = T,
                           removeWords=c("efta","liechenstein", "turkey","iceland", "efta","chapter", "annexes", "annex", "protocol","protocols")
                           ,stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))



#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")




wordcloud(colnames(dtm.matrix), dtm.matrix[5, ], max.words = 150)



wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")

require(cluster)
fviz_nbclust(as.matrix(distMatrix), kmeans, method = "silhouette")

#########################

set.seed(Sys.time())
cluster_sim= rep(0, 1000)
for (i in 1:1000){
cl1 <- kmeans(distMatrix, 6)$cluster
cl2 <- kmeans(distMatrix, 6)$cluster
cluster_sim[i]= cluster_similarity(cl1, cl2)
}


cluster_sim= rep(0, 1000)
for (i in 1:1000){
  cl1 <- kmeans(distMatrix, 6)$cluster
  cl2 <- kmeans(distMatrix, 6)$cluster
  cluster_sim[i]= cluster_similarity(cl1, cl2)
}



#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_5.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3/clustering_5.csv")

#################################
cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)

#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3/clustering_6.csv")

#################################

cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3/clustering_7.csv")

#################################

#################################

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3/clustering_8.csv")

#################################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3/clustering_9.csv")

#################################


cl <- kmeans(distMatrix, 10)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_10.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_10.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_10= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_10)= c("text", "cluster")
write.csv(pure_cl_10, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3/clustering_10.csv")

#################################



#######################Results after removing annexes with tfidf

#######################################

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
library(factoextra)

path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.05
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .9
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 20),
                           removePunctuation = T,
                           removeNumbers = T,
                           removeWords=c("efta","liechenstein", "turkey","iceland", "efta","chapter", "annexes", "annex", "protocol","protocols")
                           ,stemming = T,
                           weighting= weightTfIdf,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))



#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


wordcloud(colnames(dtm.matrix), dtm.matrix[1, ], max.words = 150)



wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")
#########################

cl <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_5.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed_tfidf/clustering_5.csv")

#################################
cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed_tfidf/clustering_6.csv")

#################################

cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed_tfidf/clustering_7.csv")

#################################

#################################

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed_tfidf/clustering_8.csv")

#################################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed_tfidf/clustering_9.csv")

#################################


cl <- kmeans(distMatrix, 10)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_10.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed_tfidf_10.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_10= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_10)= c("text", "cluster")
write.csv(pure_cl_10, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed_tfidf/clustering_10.csv")

#################################





#######################################STEMMED 3 w/o annexes - it is close to 1 

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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .85
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 20),
                           removePunctuation = T,
                           removeNumbers = T,
                           removeWords=c("efta","liechenstein", "turkey","iceland", "efta","chapter", "annexes", "annex", "protocol","protocols")
                           ,stemming = T,
                           weighting= weightTfIdf,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))



#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


wordcloud(colnames(dtm.matrix), dtm.matrix[7, ], max.words = 50)



wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")
#########################


cl1 <- kmeans(distMatrix, 5)
cl2  <- kmeans(distMatrix, 5)


cls=as.data.frame(cl$cluster)




write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf.csv")
#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf.csv")

abc= str_split_fixed(df$X, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cl.cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3_tfidf/clustering_5.csv")

#################################
cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3_tfidf/clustering_6.csv")

#################################

cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3_tfidf/clustering_7.csv")

#################################

#################################

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3_tfidf/clustering_8.csv")

#################################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3_tfidf/clustering_9.csv")

#################################


cl <- kmeans(distMatrix, 10)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_10.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed3_tfidf_10.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_10= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_10)= c("text", "cluster")
write.csv(pure_cl_10, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed3_tfidf/clustering_10.csv")

#################################

































###########DIDNOT WORK THAT WELL



#######################################Cluster 6 -STEMMING 4 - close to the first steeming 0.05 and 0.9

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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.025
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .975
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 20),
                           removePunctuation = T,
                           removeNumbers = T,
                           removeWords=c("efta","liechenstein", "turkey","iceland", "efta","chapter", "annexes", "annex", "protocol","protocols")
                           ,stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))



#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


wordcloud(colnames(dtm.matrix), dtm.matrix[1, ], max.words = 150)



wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(distMatrix, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 


fviz_nbclust(as.matrix(distMatrix), kmeans, method = "gap_stat")
#########################

cl <- kmeans(distMatrix, 4)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed4_4.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed4_4.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_4= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_4)= c("text", "cluster")
write.csv(pure_cl_4, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed4/clustering_4.csv")



#########################

cl <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed4_5.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed4_5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed4/clustering_5.csv")







########################################STEMMED_2_wo_annexes----Not better than the first one w/o annexes 

#######################################Cluster 6 

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


path= "C:/Users/narendr/Dropbox"
dir = DirSource(paste(path,"/macro_wo_annexes/",sep=""), encoding = "")
corpus = Corpus(dir)


ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.1
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .8
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 20),
                           removePunctuation = T,
                           removeNumbers = T,
                           removeWords=c("efta","liechenstein", "turkey","iceland", "efta","chapter", "annexes", "annex", "protocol","protocols")
                           ,stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))



#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

inspect(dtm)

#dtm <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix = as.matrix(dtm)


m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")


wordcloud(colnames(dtm.matrix), dtm.matrix[4, ], max.words = 50)
#########################

cl <- kmeans(distMatrix, 5)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_5.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_5.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_5= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_5)= c("text", "cluster")
write.csv(pure_cl_5, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed2/clustering_5.csv")

#################################
cl <- kmeans(distMatrix, 6)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_6.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_6.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_6= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_6)= c("text", "cluster")
write.csv(pure_cl_6, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed2/clustering_6.csv")

#################################

cl <- kmeans(distMatrix, 7)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_7.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_7.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_7= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_7)= c("text", "cluster")
write.csv(pure_cl_7, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed2/clustering_7.csv")

#################################

#################################

cl <- kmeans(distMatrix, 8)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_8.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_8.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_8= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_8)= c("text", "cluster")
write.csv(pure_cl_8, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed2/clustering_8.csv")

#################################

cl <- kmeans(distMatrix, 9)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_9.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_9.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_9= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_9)= c("text", "cluster")
write.csv(pure_cl_9, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed2/clustering_9.csv")

#################################


cl <- kmeans(distMatrix, 10)
cls=as.data.frame(cl$cluster)


#write
write.csv(cls, "C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_10.csv")

#read it back again with the column names- "text" and "cluster"
df= read.csv("C:/Users/narendr/Dropbox/EIADatabaseApril2017/pure_cluster_wo_annexes_stemmed2_10.csv")
abc= str_split_fixed(df$text, "\\.", 2)
pure_cl_10= as.data.frame(cbind(abc[,1], df$cluster))
colnames(pure_cl_10)= c("text", "cluster")
write.csv(pure_cl_10, "C:/Users/narendr/Dropbox/RestrictionsTest3/Pure_Clustering_wo_annexes_stemmed2/clustering_10.csv")

#################################



