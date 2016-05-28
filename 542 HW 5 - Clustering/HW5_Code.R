# STAT 542 Assignment 5 kakilai2
#install.packages("ISLR")

setwd("/Users/EricaLai/Desktop/542")
rm(list=objects())
#library(ISLR)
#library(cluster)
#library(plyr)
library(HMM)
library(plyr)
library(hmm.discnp)

# Question 1
seq1 = scan("f15_hw4_seq1.txt")
seq2 = scan("f15_hw4_seq2.txt")
seq3 = scan("f15_hw4_seq3.txt")
fit1 = list()
aic1 = list()

#Seq 1
cat("Sequence 1: \n")
for (i in 1:3) {
  
  fit1[[i]] = hmm(seq1, K = i,itmax = 1000)
  aic1[[i]] = -2*fit1[[i]]$log.like+2*(i^2+2*i-1)
  cat("AIC of k = ", i, ":", aic1[[i]],"\n")
}

for (i in 2:3) {
  cat("Increase of AIC (%) when K = ",i,":",(aic1[[i]] - aic1[[i-1]])/aic1[[i-1]]*100,"\n" )
  
}

#Seq 2
fit2 = list()
aic2 = list()
cat("Sequence 2: \n")
for (i in 1:3) {
  
  fit2[[i]] = hmm(seq2, K = i,itmax = 1000)
  aic2[[i]] = -2*fit2[[i]]$log.like+2*(i^2+2*i-1)
  cat("AIC of k = ", i, ":", aic2[[i]],"\n")
}

for (i in 2:3) {
  cat("Increase of AIC (%) when K = ",i,":",(aic2[[i]] - aic2[[i-1]])/aic2[[i-1]]*100,"\n" )
}

#Seq 3
fit3 = list()
aic3 = list()
cat("Sequence 3: \n")
for (i in 1:3) {
  
  fit3[[i]] = hmm(seq3, K = i,itmax = 1000)
  aic3[[i]] = -2*fit3[[i]]$log.like+2*(i^2+2*i-1)
  cat("AIC of k = ", i, ":", aic3[[i]],"\n")
}

for (i in 2:3) {
  cat("Increase of AIC (%) when K = ",i,":",(aic3[[i]] - aic3[[i-1]])/aic3[[i-1]]*100,"\n" )
}

# 2 hidden states for Sequence 1
fit1[[2]]$Rho
fit1[[2]]$tpm

# 1 hidden states for Sequence 2
fit2[[1]]$Rho
fit2[[1]]$tpm

# 3 hidden states for Sequence 3
fit3[[3]]$Rho
fit3[[3]]$tpm

nci.labs=NCI60$labs
nci.data=NCI60$data
sd.data=scale(nci.data)
D = dist(sd.data)

# a) K-means clustering
best_score = 0
best_num_cluster = 0
s_list = list()
for (i in 2:10) {
  kms = kmeans(sd.data, centers=i, nstart=20)
  s = silhouette(kms$cluster, D)
  average_s = mean(s[,3])
  s_list = cbind(s_list,average_s)
  if (average_s > best_score) {
    best_score = average_s
    best_kms = kms
    best_num_cluster = i
    best_s = s
  }
}

plot(2:10,s_list,type="o",xlab='K', ylab='Average Silhouette')
title("Average Silhouette")

plot(best_s)

dis=cmdscale(D);
par(mfrow=c(1,1));
plot(dis[,1], dis[,2], type = 'n');
points(dis[,1], dis[,2], pch=best_kms$cluster, col=best_kms$cluster);
title(c("K-Means result by MDS,K = ", best_num_cluster))


# Consider K* = 4
kms_4 = kmeans(sd.data, centers=4, nstart=20)
s4 = silhouette(kms_4$cluster, D)
plot(s4)
plot(dis[,1], dis[,2], type="n");
points(dis[,1], dis[,2], pch=kms_4$cluster, col=kms_4$cluster);
title("K-Means result by MDS,K = 4")


# b)  Average linkage hierarchical clustering

ave_hie = hclust(D, method="average")
plclust(ave_hie, xlab="Average Linkage", sub="")

best_score = 0
best_num_cluster = 0
best_clust = 0 
s_list = list()

for (i in 2:10) {
  clus = cutree(ave_hie, k = i)
  s = silhouette(clus, D)
  average_s = mean(s[,3])
  s_list = cbind(s_list,average_s)
  if (average_s > best_score) {
    best_score = average_s
    best_kms = kms
    best_num_cluster = i
    best_s = s
    best_clus = clus
  }
}

plot(2:10,s_list,type="o",xlab='K', ylab='Average Silhouette')
title("Average Silhouette")
plot(best_s)


#par(mfrow=c(1,1));
plot(dis[,1], dis[,2], type = 'n');
points(dis[,1], dis[,2], pch=best_clus, col=best_clus);
title(c("Hierarchical clustering result by MDS,K = ", best_num_cluster))

# c) check on whether two clustering results agree with the cancer types
par(mfrow=c(3,1));

cancer_type = as.numeric(factor(nci.labs))
plot(dis[,1], dis[,2], type = 'n', main= 'Distribution of cancer_type');
points(dis[,1], dis[,2], pch=cancer_type, col=cancer_type)

# for K-Means 9 clusters

plot(dis[,1], dis[,2], type = 'n', main = 'K-Means 9 clusters');
points(dis[,1], dis[,2], pch=best_kms$cluster, col=best_kms$cluster);


# for K-Means 4 clusters

# plot(dis[,1], dis[,2], type = 'n', main = 'K-Means 4 clusters');
# points(dis[,1], dis[,2], pch=kms_4$cluster, col=kms_4$cluster);

# for Hierarchical

plot(dis[,1], dis[,2], type = 'n', main = 'Hierarchical 2 clusters');
points(dis[,1], dis[,2], pch=best_clus, col=best_clus);


#precision and recall
clus_k9 = best_kms$cluster
clus_k4 = kms_4$cluster
clus_h2 = best_clus

# for K-Means 9 clusters
clus = clus_k9
tp=0
fp=0
fn=0
tn=0
for (i in 1:64) {
  for (j in 1:64) {
    if (clus[i] == clus[j]) {
      if (nci.labs[i] == nci.labs[j]) {
        tp = tp+1
      }
      else {
        fp = fp+1
      }
    }
    else {
      if (nci.labs[i] != nci.labs[j]) {
        tn = tn+1
      }
      else {
        fn = fn+1
      }
    }
  }
}
precision_k9 = tp/(tp+fp)
recall_k9 = tp/(tp+fn)
f1_k9 = (2*precision_k9*recall_k9)/(precision_k9+recall_k9)

# for K-Means 4 clusters
clus = clus_k4
tp=0
fp=0
fn=0
tn=0
for (i in 1:64) {
  for (j in 1:64) {
    if (clus[i] == clus[j]) {
      if (nci.labs[i] == nci.labs[j]) {
        tp = tp+1
      }
      else {
        fp = fp+1
      }
    }
    else {
      if (nci.labs[i] != nci.labs[j]) {
        tn = tn+1
      }
      else {
        fn = fn+1
      }
    }
  }
}
precision_k4 = tp/(tp+fp)
recall_k4 = tp/(tp+fn)
f1_k4 = (2*precision_k4*recall_k4)/(precision_k4 + recall_k4)



# for Hierarchical with 2 clusters
clus = clus_h2
tp=0
fp=0
fn=0
tn=0
for (i in 1:64) {
  for (j in 1:64) {
    if (clus[i] == clus[j]) {
      if (nci.labs[i] == nci.labs[j]) {
        tp = tp+1
      }
      else {
        fp = fp+1
      }
    }
    else {
      if (nci.labs[i] != nci.labs[j]) {
        tn = tn+1
      }
      else {
        fn = fn+1
      }
    }
  }
}
precision_h2 = tp/(tp+fp)
recall_h2 = tp/(tp+fn)
f1_h2 = (2*precision_h2*recall_h2)/(precision_h2 + recall_h2)



# Print result:

cat("Precision for K-Means with 9 clusters", precision_k9,"\n"
,"Recall for K-Means with 9 clusters", recall_k9,"\n"
,"F-Score for K-Means with 9 clusters", f1_k9, "\n"
,"Precision for Hierarchical with 4 clusters", precision_h2,"\n"
,"Recall for Hierarchical with 2 clusters", recall_h2,"\n"
,"F-Score for Hierarchical with 2 clusters", f1_h2, "\n")
