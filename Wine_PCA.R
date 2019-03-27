wine <- read.table("~/projects/wine.data", header=FALSE, sep=",")
str(wine)
colnames(wine)<-c("class", "Alcohol", "Malic_Acid", "Ash",  "Alcanity", "Magnesium", "phenols", "Flavanoids", "Non_Flav_Phenols", "Proanthocynanins", "color_intensity", "Hue", "OD280", "proline")

vars.to.use<-colnames(wine)[-1]
pmatrix<-scale(wine[,vars.to.use])
pcenter<-attr(pmatrix, 'scaled:center')
pcale<-attr(pmatrix, "scaled:scale")

d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D2")
plot(pfit, labels=wine$class)
rect.hclust(pfit, k=3)


groups <- cutree(pfit, k=3)
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(wine[labels==i,c("class", "Alcohol", "Malic_Acid", "Ash",  "Alcanity", "Magnesium", "phenols", "Flavanoids", "Non_Flav_Phenols", "Proanthocynanins", "color_intensity", "Hue", "OD280", "proline")])
  }
}
print_clusters(groups, 3)


library(ggplot2)
#PCA
princ <- prcomp(pmatrix)
nComp <- 7

library(ggbiplot)
summary(princ)
#Should probably use more principal components because first two only explain 55%
ggbiplot(princ)
ggbiplot(princ,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=wine$class, groups=wine$class)
