# Load the data
set.seed(123)
data <- read.csv("C:/Users/dsl89/Documents/multiVar/project/Global Inflation Dataset.csv", row.names = "Country")
head(data)


# Fill the missing values with median
for(q in 1:ncol(data)){
  NAs <- is.na(data[, q])
  NA_index <- which(NAs)
  data[NA_index, q] <- median(data[, q], na.rm = TRUE)
}
head(data)
dataS <- scale(data)    #scale data

head(dataS)

library("mclust")
dataD <- dist(dataS)           # euclidean Distance 
head(dataD)
dataD

# K-means find number of cluster with function

plot.wgss = function(data, maxc = nrow(data) - 1) {
  wgss = numeric(maxc)
  for (i in 1:maxc) {
    km <- kmeans(data, centers = i, nstart = 10)
    wgss[i] = km$tot.withinss
  }
  plot(1:maxc, wgss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares",
       main = "K-Means Scree Plot")
}
plot.wgss(dataS, maxc = 30)      # Elbow test appears to be around 5 maybe 4

library(knitr)                   # Kmeans library
km <- kmeans(dataS, centers = 5) # Applying kmeans for k=5 clusters
table(km$cluster)                # count table
head(km$cluster)

# belonging to what cluster
subset(dataS, km$cluster == 1) 
subset(dataS, km$cluster == 2)
subset(dataS, km$cluster == 3)
subset(dataS, km$cluster == 4)
subset(dataS, km$cluster == 5)

# PCA and Score plot Two princ Components

km <- kmeans(dataS, centers = 5, nstart = 10)
pca <- princomp(dataS)
pca$loadings[,1:2]  # How does PC and PC2 relate?

plot(pca$scores[, 1:2], col = "white", main = "PCA Biplot - Kmean Clustering")
text(pca$scores[, 1:2], labels = abbreviate(row.names(data)), col = km$cluster,
     cex = 0.5)

# WGSS: within-group sum of squares smaller is better 4 is higher than 454.5479
km$tot.withinss

# column means table
clusterMeans <- km$centers
rownames(clusterMeans) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")
library(knitr)
kable(clusterMeans, caption = "Column Means for each Cluster")

# Hireacial Clustering

# HC - average
hc <- hclust(dataD, method = "average")           #Hierarchical Cluster, method average
hcAvg <- cutree(hc, k = 5)     # specify number of Clusters, cut tree
table(hcAvg)                   
            
# HC - single
hc <- hclust(dataD, method = "single")           #Hierarchical Cluster, method single
hcSingle <- cutree(hc, k = 5)     # specify number of Clusters, cut tree
table(hcSingle)                      

# HC - complete
                   
hc <- hclust(dataD, method = "complete")       
hc$height                                                  #height values
plot(rev(hc$height), main = "Hierarchical Scree Plot" )   

plot(hc, main = "Complete Linkage HC Dendogram", cex = .4)    # Dendogram of metrics
abline(h = 5, col = "red")
library(knitr)
ct <- cutree(hc, k = 5)     # specify number of Clusters, cut tree
table(ct)                   #cutree table assigned height=5
ct

# Report the mean variable value of each cluster. Interpret and assign a label to each cluster. 
cluster1 = subset(rownames(data), ct == 1)            
index1 = match(cluster1, rownames(data))
clust1mean = colMeans(dataS[index1, ])

cluster2 = subset(rownames(data), ct == 2)
index2 = match(cluster2, rownames(data))
clust2mean = colMeans(dataS[index2, ])

cluster3 = subset(rownames(data), ct == 3)
index3 = match(cluster3, rownames(data))
clust3mean = colMeans(dataS[index3, ])

clustMeans <- rbind(clust1mean, clust2mean, clust3mean)
kable(clustMeans, caption = "Column Means for Each Cluster")

# PCA and Score plot Two princ Components by HC

plot(pca$scores[, 1:2], col = "white", main = "PCA Biplot - Hierarchical Clustering")
text(pca$scores[, 1:2], labels = abbreviate(row.names(dataS)), col = ct,
     cex = 0.5)        # K means is clearer than HC

#Model Based check model based suggestion of how many groups

library("mclust")
mc <- Mclust(data)      # takes a long time but close to kmeans
table(mc$classification)   #according to this there appears to be four groups similar to k means
mc$classification

plot(mc, what = "BIC")
summary(mc$BIC)

#Chi sq test for MC 
tbMC = table(mc$classification, row.names(data)) #contingency table for  mc, pick
chisq.test(tbMC)  

# #Chi sq test for KM
tbKM = table(km$cluster, row.names(data)) #contingency table for  km, pick
chisq.test(tbKM) 

# #Chi sq test for HC
tbHC = table(ct, row.names(data)) #contingency table for  hc, pick
chisq.test(tbHC) 


# Analysis and uncertainity

table(km$cluster)           # kmeans
table(hcComp)               # hC
table(mc$classification)    # model

#               what countries go which group? What does clusters mean
plot(pca$scores[, 1:2], col = "white", main = "PCA Biplot - Kmean Clustering")
text(pca$scores[, 1:2], labels = abbreviate(row.names(eS)), col = km$cluster,
     cex = 0.5)
#           Find uncertain points, darker and larger points are more uncertain
plot(mc, what = "uncertainty")

#                      Check Uncertainty Values
clust.data = cbind(rownames(data), mc$classification, mc$uncertainty)
sorted_uncertainty <- clust.data[order(-mc$uncertainty), ]
head(sorted_uncertainty, 10)










