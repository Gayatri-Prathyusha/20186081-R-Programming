dailykos=read.csv("dailykos.csv")
kosDist=dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method = "ward.D")
plot(kosHierClust)
heirgroups = cutree(kosHierClust, k=7)
table(heirgroups)
tail(sort(colMeans(heircluster1)))
tail(sort(colMeans(heircluster2)))
tail(sort(colMeans(heircluster3)))
tail(sort(colMeans(heircluster4)))
tail(sort(colMeans(heircluster5)))
tail(sort(colMeans(heircluster6)))
tail(sort(colMeans(heircluster7)))

set.seed(1000)
kmeanscluster = kmeans(dailykos, centers=7)
table(kmeanscluster$cluster)
kmeanscluster1 = subset(dailykos, kmeanscluster$cluster== 1)
kmeanscluster2 = subset(dailykos, kmeanscluster$cluster== 2)
kmeanscluster3 = subset(dailykos, kmeanscluster$cluster== 3)
kmeanscluster4 = subset(dailykos, kmeanscluster$cluster== 4)
kmeanscluster5 = subset(dailykos, kmeanscluster$cluster== 5)
kmeanscluster6 = subset(dailykos, kmeanscluster$cluster== 6)
kmeanscluster7 = subset(dailykos, kmeanscluster$cluster== 7)
tail(sort(colMeans(kmeanscluster1)))
tail(sort(colMeans(kmeanscluster2)))
tail(sort(colMeans(kmeanscluster3)))
tail(sort(colMeans(kmeanscluster4)))
tail(sort(colMeans(kmeanscluster5)))
tail(sort(colMeans(kmeanscluster6 )))
tail(sort(colMeans(kmeanscluster7)))
table(heirgroups, kmeanscluster$cluster)







