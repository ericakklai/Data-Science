# PCA
# Mar 5, 2016

setwd("/Users/EricaLai/Desktop/pcapractise")
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

#Visualize the data
library('chemometrics')
library(reshape2)
library(ggplot2)
#iris_nipals = nipals(numiris,a=2)
#iris_nipals$P[,1]
pca <- prcomp(numiris, scale=T)
points = data.frame(irisdat$V5,pca$x[,1:2])
pc <- qplot(x=PC1, y=PC2, data=points, colour=factor(irisdat$V5)) +theme(legend.title = element_blank())
pc


niris = as.matrix(numiris)
pca_pls1 = pls1_nipals(niris, as.numeric(irisdat$V5), a=2)
point2 = data.frame(irisdat$V5,pca_pls1$T)
pc <- qplot(x=X1, y=X2, data=point2, colour=factor(irisdat.V5)) +theme(legend.title = element_blank())
pc


#wine data set
winedat = read.csv('wine.data.csv', header = FALSE)
x = winedat[,-1]
y = winedat[,1]
y_fac=as.factor(y)
k = prcomp(x, center = T,scale = F)
plot(1:13,(k$sdev)*2, type= 'b', main = "eigenvalues in sorted order", xlab = "PC", ylab = "eigenvales")

#
#Output files to matlab for plotting
write.csv(k$rotation, file = "pca_resultplot.csv" )

projected = scale(x, k$center, k$scale) %*% k$rotation 
plot(x = projected[,"PC1"], y = projected[,"PC2"], col = y_fac,pch=c('1','2','3')[y_fac], main= 'Scatter plot of the data on 1st and 2nd PCs', xlab = 'PC1', ylab = 'PC2')


cancer <- read.csv("~/Desktop/cancer.txt", header=FALSE)
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



pls1 = pls1_nipals(numcan,as.numeric(cancer$V11),a=3)
point4 = data.frame(cancer$V11,pls1$T)

# Scatter plot
pc$pcolor[point4$cancer.V11 == 2] = "red"
pc$pcolor[point4$cancer.V11 == 4] = "blue"
with(pc,{
  s3d = scatterplot3d(point4$X1,point4$X2,point4$X3,
                      color = pcolor)
})