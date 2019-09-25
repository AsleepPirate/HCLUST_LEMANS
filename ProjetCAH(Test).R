


##Données
setwd("C:/Users/Admin/Desktop")
WineTypes <- read.table("wine.data",sep=",",dec=".",header=F)
View(WineTypes)
summary(WineTypes)
#La premiere colonne est celle des vraies classes.
Classes <- WineTypes[,1]
WineTypes[,1] <- NULL
colnames(WineTypes) <- c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                         "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins",
                         "Color intensity","Hue","OD280/OD315","Proline")
#Standardisation

head(WineTypes)
SWineTypes <- scale(WineTypes)
head(SWineTypes)

##Classification ascendante hiérarchique

#Matrice de distance:
d <- dist(SWineTypes, method = "euclidean")
#CAH
hc1 <- hclust(d, method = "complete" )
library(cluster)
hc2 <-diana(SWineTypes)
#Dendogramme:
plot(hc1,cex=0.7)

#Clustering:
TwoClusters = cutree(hc1 ,2)
ThreeClusters = cutree(hc1 ,3)
FourClusters = cutree(hc1 ,4)

table(TwoClusters)
table(ThreeClusters)
table(FourClusters)

DTwoClusters = cutree(hc2 ,2)
DThreeClusters = cutree(hc2 ,3)
DFourClusters = cutree(hc2 ,4)

table(DTwoClusters)
table(DThreeClusters)
table(DFourClusters)


plot(hc1, cex = 0.7)
rect.hclust(hc1, k = 2, border = 2:5)
rect.hclust(hc1, k = 3, border = 2:5)
rect.hclust(hc1, k = 4, border = 2:5)

#Visualisation du resultat sur les deux composantes principales

library(factoextra)
#Données réeles:
fviz_cluster(list(data = SWineTypes, cluster = Classes))
#CAH
fviz_cluster(list(data = SWineTypes, cluster = ThreeClusters))
fviz_cluster(list(data = SWineTypes, cluster = DThreeClusters))


