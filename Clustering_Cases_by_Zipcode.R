library(readxl)
city_data2 <-read_excel("Zip_code_all_normalized.xlsx")

library(GGally)
library(ggplot2)

ds2<- city_data2[3:14]
ggcorr(ds2, palette = "RdBu", label = TRUE, hjust = 1, size = 4.5,layout.exp = 6)

#Clustering

library(rattle)
head(city_data2)
df2 <- scale(city_data2[3:14])
head(df2)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(df2)

fit.km2 <- kmeans(df2,3)
fit.km2$size

aggregate(city_data2[3:14],by=list(cluster=fit.km2$cluster),median)

ct.km2<- table(city_data2$City_Name,fit.km2$cluster)
ct.km2

install.packages("flexclust")        
library(flexclust)
randIndex(ct.km2)


dd <- cbind(city_data2, cluster = fit.km2$cluster)
head(dd)

fit.km$size

library(cluster)
fit.pam <- pam(city_data[-1],k=3,stand=TRUE)
clusplot(fit.pam,main="Cluster analysis")


clust_mean <- aggregate(city_data2[3:14],by=list(cluster=fit.km2$cluster),mean)

clust_median <- aggregate(city_data2[3:14],by=list(cluster=fit.km2$cluster),median)
clust_median

write.csv(clust_mean,file="Cluster_means.csv")
write.csv(clust_median,file="Cluster_medians.csv")

write.csv(dd,file="Clustered_cases_all.csv")


city_data.pca <- prcomp(city_data2[,c(3:14)], center = TRUE,scale. = TRUE)
city_data.pca

