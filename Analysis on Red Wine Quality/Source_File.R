# loading necessary libraries
library(psych)
library(readr)
library(NbClust)
library(factoextra) 
library("ggcorrplot")
library("ggplot2")
library("cluster")
library(dendextend)

# setwd("/Users/knvardhan/Downloads/Regression/Project\ 1")

data1 <- read_delim("winequality-red.csv",show_col_types=FALSE)
data1


# Correlation Matrix
corr_matrix = cor(data1)
ggcorrplot(corr_matrix)

# Principal Component Analysis
fa.parallel(data1, n.iter = 100, fa="pc", main="Scree Plot - PCA")

data1.pca <- princomp(corr_matrix)
summary(data1.pca)

pca <- principal(data1, nfactors = 4)
pca

pcadf <- data.frame(pca$scores)
pcadf

# Factor Analysis
fa.parallel(corr_matrix, n.obs = 1599, n.iter = 100, fa="fa", main="Scree Plot - FA")
fa(corr_matrix, nfactors = 5, rotate="none", fm="pa")
fa(corr_matrix,  nfactors = 5,rotate="varimax", fm="pa")


## Cluster analysis
# Hierarchical clustering
nd=data1[1:100, ]
z=nd[,-c(1,1)]
means=apply(z,2,mean)
sds=apply(z,2,sd)
nor=scale(z,center=means,scale=sds)

distance = dist(nor)

nd.hclust = hclust(distance)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(nd.hclust)
avg_col_dend <- color_branches(avg_dend_obj, k =4)
plot(avg_col_dend)


# Clustering in R
k2=kmeans(nor,centers=4,nstart=100)
str(k2)
fviz_cluster(k2, data = nor)


