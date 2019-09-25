setwd("C:/Users/Admin/Desktop")

data <- read.table("wdbc.data",sep=",",dec=".",header=F)
View(data)


Classes <- data[,c(1,2)]
data[,c(1,2)] <- NULL
colnames(data)=c("Radius1","Texture1","Perimeter1","Area1","Smoothness1","Compactness1","Concavity1",
                 "Concave points1","Symmetry1","Fractal dimension1","Radius2","Texture2","Perimeter2",
                 "Area2","Smoothness2","Compactness2","Concavity2","Concave points2","Symmetry2",
                 "Fractal dimension2","Radius3","Texture3","Perimeter3","Area3","Smoothness3",
                 "Compactness3","Concavity3","Concave points3","Symmetry3","Fractal dimension3")

cdata=scale(data)
d=dist(cdata,method="euclidean")
hc=hclust(d, method = "complete" )
hc2=hclust(d, method = "ward.D" )
hc3=hclust(d, method = "single" )
hc4=hclust(d, method = "average" )
par(mfrow=c(2,1))
plot(hc)
plot(hc2)
summary(cutree(hc ,2))
summary(Classes)


library(factoextra)
gwt=cutree(hc2 ,2)
gwt1=cutree(hc ,2)
gwt2=cutree(hc3 ,2)
gwt3=cutree(hc4 ,2)

#Données réeles:
x()
fviz_cluster(list(data = cdata, cluster = Classes$V2))
x()
fviz_cluster(list(data = cdata, cluster = gwt))
#CAH
fviz_cluster(list(data = cdata, cluster = gwt))
fviz_cluster(list(data = cdata, cluster = gwt1))

fviz_cluster(list(data = cdata, cluster = gwt2))
fviz_cluster(list(data = cdata, cluster = gwt3))
