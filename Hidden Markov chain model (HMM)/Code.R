
#install.packages("ISLR")

setwd("/Users/EricaLai/Desktop/HMM")
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