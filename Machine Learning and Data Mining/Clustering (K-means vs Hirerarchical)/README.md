# Clustering (K-means vs Hirerarchical)

• Data Description 
2. This analysis uses the NCI60 microarray data from the ISLR package. 

'''sh
library("ISLR")
nci.labs=NCI60$labs; nci.data=NCI60$data
sd.data=scale(nci.data)
'''

• Summary of Analysis
(a) Cluster the samples (columns) using K-means and hierarchical clustering, and use the silhouette
statistic to estimate the number of clusters. 
(b) Use PCA to display the data
(c) Apply silhouette in package cluster to find best number of clusters.
(d) Study and Analysis on whether two clustering results agree with the cancer types.

