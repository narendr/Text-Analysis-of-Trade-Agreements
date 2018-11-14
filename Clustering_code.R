
#----------------------------------------------------------------------------------------------------------
set.seed(2)
library(tidyr)
library(plot3D)
library(cluster)


data = read.csv("C:/Users/narendr/Desktop/Trade_Agreements_Quantification/Scores_norm.csv")

#new_data <- data %>%
#  group_by(tradeagreements) %>%
#  summarise(ADCVM_sum = sum(adcvm), Agriculture_sum= sum(agriculture), Capital = sum(capitalmobility), Competition= sum(competition), Customs= sum(customsadministration), 
#            Environment= sum(environment), ExportRestrictions= sum(exportrestrictions), ImportRestrictions= sum(importrestrictions),Investment= sum(investment), 
#            IPR_sum= sum(ipr), PublicProcurement= sum(publicprocurement), Services_sum= sum(services), SPS_sum= sum(sps))


#new_data= new_data[new_data$tradeagreements!= "EEA_annex"]
#rownames(df)= new_data$tradeagreements


#distance <- get_dist(df)
new= data[-1]
set.seed(200)
k2 <- kmeans(data[-1],  centers=8, nstart=25)
str(k2)
fviz_cluster(k2, data=data[-1], show.clust.cent = TRUE, ellipse = TRUE,ellipse.level=.1,ellipse.type="convex", repel= FALSE, ellipse.alpha= 0.05, pointsize= .2, labelsize= 7, outlier.shape=0)


# plots to compare
#p1 <- fviz_cluster(k2, geom = "point", data = data[-1]) + ggtitle("k = 2")
#p2 <- fviz_cluster(k3, geom = "point",  data = data[-1]) + ggtitle("k = 3")
#p3 <- fviz_cluster(k4, geom = "point",  data = data[-1]) + ggtitle("k = 4")
#p4 <- fviz_cluster(k5, geom = "point",  data = data[-1]) + ggtitle("k = 5")


library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

wss <- (nrow(new)-1)*sum(apply(new,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(new, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 

par(mfrow=c(1,2))

fviz_nbclust(new, kmeans, method = "wss")

#------------------------------------
fviz_nbclust(new, kmeans, method = "gap_stat")

#---Clustering with k=6 
final <- kmeans(new, 6, nstart = 25)
print(final)

data$tradeagreements[final$cluster==1]
data$tradeagreements[final$cluster==2]
data$tradeagreements[final$cluster==3]
data$tradeagreements[final$cluster==4]
data$tradeagreements[final$cluster==5]
data$tradeagreements[final$cluster==6]
set.seed(2)
#Clustering with k= 9
final_9 <- kmeans(new, 9, nstart = 25)
print(final_9)
final_9$clustering

data$tradeagreements[final_9$cluster==1]
data$tradeagreements[final_9$cluster==2]
data$tradeagreements[final_9$cluster==3]
data$tradeagreements[final_9$cluster==4]
data$tradeagreements[final_9$cluster==5]
data$tradeagreements[final_9$cluster==6]
data$tradeagreements[final_9$cluster==7]
data$tradeagreements[final_9$cluster==8]
data$tradeagreements[final_9$cluster==9]

#pam_6 <- pam(new, 6)
#print(pam_6)


##########################
set.seed(2)
library(tidyr)
library(plot3D)
library(cluster)


data= read.csv("C:/Users/narendr/Dropbox/RestrictionsTest3/Final_Test_KNN_6_distance/mastering_score.csv", header=T)

new= data[-1]

k2 <- kmeans(new,  centers=9, nstart=25)
str(k2)
fviz_cluster(k2, data=data[-1], show.clust.cent = TRUE, ellipse = TRUE,ellipse.level=.1,ellipse.type="convex", repel= FALSE, ellipse.alpha= 0.05, pointsize= .2, labelsize= 7, outlier.shape=0)



k3 <- kmeans(data[-1], centers = 3, nstart = 25)
k4 <- kmeans(data[-1], centers = 4, nstart = 25)
k5 <- kmeans(data[-1], centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data[-1]) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data[-1]) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data[-1]) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data[-1]) + ggtitle("k = 5")


library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

wss <- (nrow(new)-1)*sum(apply(new,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(new, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", lwd= 2) 

par(mfrow=c(1,2))

fviz_nbclust(new, kmeans, method = "wss")

#------------------------------------
fviz_nbclust(new, kmeans, method = "gap_stat")


#########################NEW CLUSTERING AFTER BETTER TRAINING


#---Clustering with k=8
data= read.csv("C:/Users/narendr/Dropbox/RestrictionsTest3/Final_KNN_7_uniform_300words/mastering_score_300.csv", header=T)
data_new= subset(data, data$text %in% fta_texts)
new= data_new[-1]

##################################################
n= 10
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)
full_df$cl10 <- as.numeric(full_df$cluster==10)
write.csv(full_df,"knn_7_uniform_cluster_10.csv")


#############
n=9
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)
write.csv(full_df,"knn_7_uniform_cluster_9.csv")



#######


n=8
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)

write.csv(full_df,"knn_7_uniform_cluster_8.csv")



############
  
n=7
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)

write.csv(full_df,"knn_7_uniform_cluster_7.csv")

######################
n=6
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)

write.csv(full_df,"knn_7_uniform_cluster_6.csv")


######################
n=5
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)


write.csv(full_df,"knn_7_uniform_cluster_5.csv")


############################
n=4
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)

write.csv(full_df,"knn_7_uniform_cluster_4.csv")


####################################################################

#---Clustering with k=8
data= read.csv("C:/Users/narendr/Dropbox/RestrictionsTest3/Final_KNN_7_uniform_300words/mastering_score_300.csv", header=T)
data_new= subset(data, data$text %in% fta_texts)
new= data_new[-1]

##################################################
n= 10
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)
full_df$cl10 <- as.numeric(full_df$cluster==10)
write.csv(full_df,"knn_7_uniform_cluster_10.csv")


#############
n=9
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)
write.csv(full_df,"knn_7_uniform_cluster_9.csv")



#######


n=8
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)

write.csv(full_df,"knn_7_uniform_cluster_8.csv")



############

n=7
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)

write.csv(full_df,"knn_7_uniform_cluster_7.csv")

######################
n=6
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)

write.csv(full_df,"knn_7_uniform_cluster_6.csv")


######################
n=5
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)


write.csv(full_df,"knn_7_uniform_cluster_5.csv")


############################
n=4
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)

write.csv(full_df,"knn_7_uniform_cluster_4.csv")





#################################################################################

#############################


data= read.csv("C:/Users/narendr/Dropbox/RestrictionsTest3/Final_KNN_7_uniform_325words/mastering_score_325.csv", header=T)
data_new= subset(data, (data$text %in% fta_texts))
new= data_new[-1]

fviz_nbclust(new, kmeans, method = "wss")

fviz_nbclust(new, kmeans, method = "gap_stat")

#############################
n=8
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)

write.csv(full_df,"knn_7_uniform_cluster325_8.csv")



############

n=7
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)

write.csv(full_df,"knn_7_uniform_cluster325_7.csv")

######################
n=6
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)

write.csv(full_df,"knn_7_uniform_cluster325_6.csv")


######################
n=5
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)


write.csv(full_df,"knn_7_uniform_cluster325_5.csv")


############################
n=4
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)

write.csv(full_df,"knn_7_uniform_cluster325_4.csv")




####################################### KNN Uniform 7 averaging 275 , 300 and 325 words 

data= read.dta13("C:/Users/narendr/Dropbox/RestrictionsTest3/Final_KNN_7_uniform_300average/mastering_300_knn_average.dta")
data_new= subset(data, data$text %in% fta_texts)
new= data_new[-1]

fviz_nbclust(new, kmeans, method = "wss")

fviz_nbclust(new, kmeans, method = "gap_stat")

##########################
#############################
n=11
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)
full_df$cl10 <- as.numeric(full_df$cluster==10)
full_df$cl11 <- as.numeric(full_df$cluster==11)

write.csv(full_df,"knn_7_uniform_cluster300average_11.csv")


###################
##########################
#############################
n=10
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)
full_df$cl10 <- as.numeric(full_df$cluster==10)

write.csv(full_df,"knn_7_uniform_cluster300average_10.csv")


#############################
n=9
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)


write.csv(full_df,"knn_7_uniform_cluster300average_9.csv")




###################
n=8
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)

write.csv(full_df,"knn_7_uniform_cluster300average_8.csv")



############

n=7
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)

write.csv(full_df,"knn_7_uniform_cluster300average_7.csv")

######################
n=6
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)

write.csv(full_df,"knn_7_uniform_cluster300average_6.csv")


######################
n=5
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)

write.csv(full_df,"knn_7_uniform_cluster300average_5.csv")

##############################################
######################
n=4
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)

write.csv(full_df,"knn_7_uniform_cluster300average_4.csv")






###############################################Logit -averaging 275, 300 and 325 words


data= read.dta13("C:/Users/narendr/Dropbox/RestrictionsTest3/Final_Logit_1_l1_balanced_300_average/mastering_logit_average_300.dta")
data_new= subset(data, data$text %in% fta_texts)
new= data_new[-1]

fviz_nbclust(new, kmeans, method = "wss")

fviz_nbclust(new, kmeans, method = "gap_stat")

n=9
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)
full_df$cl9 <- as.numeric(full_df$cluster==9)


write.csv(full_df,"logit_1_l1_balanced_cluster300average_9.csv")




###################
n=8
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)
full_df$cl8 <- as.numeric(full_df$cluster==8)

write.csv(full_df,"logit_1_l1_balanced_cluster300average_8.csv")



############

n=7
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)
full_df$cl7 <- as.numeric(full_df$cluster==7)

write.csv(full_df,"logit_1_l1_balanced_cluster300average_7.csv")

######################
n=6
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)
full_df$cl6 <- as.numeric(full_df$cluster==6)

write.csv(full_df,"logit_1_l1_balanced_cluster300average_6.csv")


######################
n=5
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)
full_df$cl5 <- as.numeric(full_df$cluster==5)

write.csv(full_df,"logit_1_l1_balanced_cluster300average_5.csv")

##############################################
######################
n=4
set.seed(200)
kcluster <- kmeans(new, n, nstart = 25)

full_df={}
for (i in 1:n){
  df=as.data.frame(data_new$text[kcluster$cluster==i])
  df= cbind(df, i)
  full_df= rbind(df, full_df)
}

colnames(full_df)= c("text", "cluster")


full_df$cl1 <- as.numeric(full_df$cluster==1)
full_df$cl2 <- as.numeric(full_df$cluster==2)
full_df$cl3 <- as.numeric(full_df$cluster==3)
full_df$cl4 <- as.numeric(full_df$cluster==4)


write.csv(full_df,"logit_1_l1_balanced_cluster300average_4.csv")





