# CS498 Homework 4
#3.4 (a)
setwd("/Users/EricaLai/Desktop/CS498/HW4")
irisdat <- read.csv('iris.data.csv', header = FALSE)
library('lattice')
numiris = irisdat[,c(1,2,3,4)]
postscript("irisscatterplot.eps")

speciesnames <-c('setosa', 'versicolor', 'virginica')
pchr <-c(1,2,3)
colr <-c ('red','green','blue','yellow','orange')
ss<-expand.grid(species = 1:3)
parset <-with(ss,simpleTheme(pch=pchr[species],col=colr[species]))
print(splom(irisdat[,c(1:4)],groups = irisdat$V5, par.settings = parset, varnames = c('Sepal\nLength','Sepal\nWidth','Petal\nLength','Petal\nWidth'),key = list(text=list(speciesnames), points = list(pch=pchr), columns = 3)))
dev.off()

#3.4 (b)
library('chemometrics')
library(reshape2)
library(ggplot2)
#iris_nipals = nipals(numiris,a=2)
#iris_nipals$P[,1]
pca <- prcomp(numiris, scale=T)
points = data.frame(irisdat$V5,pca$x[,1:2])
pc <- qplot(x=PC1, y=PC2, data=points, colour=factor(irisdat$V5)) +theme(legend.title = element_blank())
pc


#3.4 (c)
niris = as.matrix(numiris)
pca_pls1 = pls1_nipals(niris, as.numeric(irisdat$V5), a=2)
point2 = data.frame(irisdat$V5,pca_pls1$T)
pc <- qplot(x=X1, y=X2, data=point2, colour=factor(irisdat.V5)) +theme(legend.title = element_blank())
pc


#3.5 (a)
winedat = read.csv('wine.data.csv', header = FALSE)
x = winedat[,-1]
y = winedat[,1]
y_fac=as.factor(y)
k = prcomp(x, center = T,scale = F)
plot(1:13,(k$sdev)*2, type= 'b', main = "eigenvalues in sorted order", xlab = "PC", ylab = "eigenvales")

#3.5 (b)
#Output files to matlab for plotting
write.csv(k$rotation, file = "assn3_Q3P5b.csv" )

#3.5 (c)
projected = scale(x, k$center, k$scale) %*% k$rotation 
plot(x = projected[,"PC1"], y = projected[,"PC2"], col = y_fac,pch=c('1','2','3')[y_fac], main= 'Scatter plot of the data on 1st and 2nd PCs', xlab = 'PC1', ylab = 'PC2')

#3.7 (a)

cancer <- read.csv("~/Desktop/498hw4/cancer.txt", header=FALSE)
numcan = cancer[,c(2:10)]
numcan = sapply(numcan,as.numeric)
pca = prcomp(numcan,scale=T)
point3 = data.frame(cancer$V11,pca$x[,1:3])

library(scatterplot3d)
pc$pcolor[point3$cancer.V11 == 2] = "red"
pc$pcolor[point3$cancer.V11 == 4] = "blue"
with(pc,{
  s3d = scatterplot3d(point3$PC1,point3$PC2,point3$PC3,
                      color = pcolor)
})


#3.7 (b)
pls1 = pls1_nipals(numcan,as.numeric(cancer$V11),a=3)
point4 = data.frame(cancer$V11,pls1$T)

pc$pcolor[point4$cancer.V11 == 2] = "red"
pc$pcolor[point4$cancer.V11 == 4] = "blue"
with(pc,{
  s3d = scatterplot3d(point4$X1,point4$X2,point4$X3,
                      color = pcolor)
})