########################################
# TRabalho de Clusterização
########################################

library(cluster)
library(NLP)
library(flexclust)
library(e1071)
library(fpc)

feature <- read.csv("C:\\features.csv", header = TRUE, sep = ",")
headline <- read.csv("C:\\\\headlines.csv", header = TRUE, sep = ",")

#Numero de dimensões da feature
dim(feature)

#redução de dimensionalidade da feature
data.pca1 <- prcomp(feature[,1:2181], scale = T)
saveRDS(data.pca1, "C:\\pca1.RDS")
data.pca1 <- readRDS("C:\\pca1.RDS")
data.pca2 <- prcomp(feature[,1:2181], scale = F)
saveRDS(data.pca2, "C:\\pca2.RDS")
data.pca2 <- readRDS("C:\\pca2.RDS")

#Verificando a variancia com scale T
vars <- apply(data.pca1$x, 2, var)
props <- vars / sum(vars);props
#85%
sum(props[1:1588])
#90%
sum(props[1:1750])
plot(cumsum(props))

#Verificando a variancia com scale F
vars <- apply(data.pca2$x, 2, var)
props2 <- vars / sum(vars)
#85%
sum(props2[1:1310])
#90%
sum(props2[1:1520])
plot(cumsum(props2))


###calculo K-means com a base completa e scale igual a T
#Variância 85%
####choosing k

k <- kmeans(data.pca1$x[,1:1588], 4, nstart = 10 )
ch <- calinhara(data.pca1$x[,1:1588], k$cluster)
saveRDS(k, "C:\\k.RDS")
saveRDS(ch, "C:\\ch.RDS")

k2 <- kmeans(data.pca1$x[,1:1588], 5, nstart = 10 )
ch2 <- calinhara(data.pca1$x[,1:1588], k2$cluster)
saveRDS(k2, "C:\\k2.RDS")
saveRDS(ch2, "C:\\ch2.RDS")

k3 <- kmeans(data.pca1$x[,1:1588], 6, nstart = 10 )
ch3 <- calinhara(data.pca1$x[,1:1588], k3$cluster)
saveRDS(k3, "C:\\k3.RDS")
saveRDS(ch3, "C:\\ch3.RDS")

k4 <- kmeans(data.pca1$x[,1:1588], 7, nstart = 10 )
ch4 <- calinhara(data.pca1$x[,1:1588], k4$cluster)
saveRDS(k4, "C:\\k4.RDS")
saveRDS(ch4, "C:\\ch4.RDS")

k5 <- kmeans(data.pca1$x[,1:1588], 8, nstart = 10)
ch5 <- calinhara(data.pca1$x[,1:1588], k5$cluster)
saveRDS(k5, "C:\\k5.RDS")
saveRDS(ch5, "C:\\ch5.RDS")

k6 <- kmeans(data.pca1$x[,1:1588], 9, nstart = 10)
ch6 <- calinhara(data.pca1$x[,1:1588], k6$cluster)
saveRDS(k6, "C:\\k6.RDS")
saveRDS(ch6, "C:\\ch6.RDS")

k7 <- kmeans(data.pca1$x[,1:1588], 10, nstart = 10)
ch7 <- calinhara(data.pca1$x[,1:1588], k7$cluster)
saveRDS(k7, "C:\\k7.RDS")
saveRDS(ch7, "C:\\ch7.RDS")

k8 <- kmeans(data.pca1$x[,1:1588], 11, nstart = 10)
ch8 <- calinhara(data.pca1$x[,1:1588], k8$cluster)
saveRDS(k8, "C:\\k8.RDS")
saveRDS(ch8, "C:\\ch8.RDS")

k9 <- kmeans(data.pca1$x[,1:1588], 12, nstart = 10)
ch9 <- calinhara(data.pca1$x[,1:1588], k9$cluster)
saveRDS(k9, "C:\\k9.RDS")
saveRDS(ch9, "C:\\ch9.RDS")

k10 <- kmeans(data.pca1$x[,1:1588], 13, nstart = 10)
ch10 <- calinhara(data.pca1$x[,1:1588], k10$cluster)
saveRDS(k10, "C:\\k10.RDS")
saveRDS(ch10, "C:\\ch10.RDS")

k11 <- kmeans(data.pca1$x[,1:1588], 14, nstart = 10)
ch11 <- calinhara(data.pca1$x[,1:1588], k11$cluster)
saveRDS(k11, "C:\\k11.RDS")
saveRDS(ch11, "C:\\ch11.RDS")

k12 <- kmeans(data.pca1$x[,1:1588], 15, nstart = 10)
ch12 <- calinhara(data.pca1$x[,1:1588], k12$cluster)
saveRDS(k12, "C:\\k12.RDS")
saveRDS(ch12, "C:\\ch12.RDS")

k13 <- kmeans(data.pca1$x[,1:1588], 16, nstart = 10)
ch13 <- calinhara(data.pca1$x[,1:1588], k13$cluster)
saveRDS(k13, "C:\\k13.RDS")
saveRDS(ch13, "C:\\ch13.RDS")

k14 <- kmeans(data.pca1$x[,1:1588], 17, nstart = 10)
ch14 <- calinhara(data.pca1$x[,1:1588], k14$cluster)
saveRDS(k14, "C:\\k14.RDS")
saveRDS(ch14, "C:\\ch14.RDS")

k15 <- kmeans(data.pca1$x[,1:1588], 18, nstart = 10)
ch15 <- calinhara(data.pca1$x[,1:1588], k15$cluster)
saveRDS(k15, "C:\\k15.RDS")
saveRDS(ch15, "C:\\ch15.RDS")

k16 <- kmeans(data.pca1$x[,1:1588], 19, nstart = 10)
ch16 <- calinhara(data.pca1$x[,1:1588], k16$cluster);ch16
saveRDS(k16, "C:\\k16.RDS")
saveRDS(ch16, "C:\\ch16.RDS")

k17 <- kmeans(data.pca1$x[,1:1588], 20, nstart = 10)
ch17 <- calinhara(data.pca1$x[,1:1588], k17$cluster)
saveRDS(k17, "C:\\k17.RDS")
saveRDS(ch17, "C:\\ch17.RDS")

k <- readRDS()
k2 <- readRDS()
k3 <- readRDS()
k4 <- readRDS()
k5 <- readRDS()
k6 <- readRDS()
k7 <- readRDS()
k8 <- readRDS()
k9 <- readRDS()
k10 <- readRDS()
k11 <- readRDS()
k12 <- readRDS()
k13 <- readRDS()
k14 <- readRDS()
k15 <- readRDS()
k16 <- readRDS()
k17 <- readRDS()

ch <- readRDS()
ch2 <- readRDS()
ch3 <- readRDS()
ch4 <- readRDS()
ch5 <- readRDS()
ch6 <- readRDS()
ch7 <- readRDS()
ch8 <- readRDS()
ch9 <- readRDS()
ch10 <- readRDS()
ch11 <- readRDS()
ch12 <- readRDS()
ch13 <- readRDS()
ch14 <- readRDS()
ch15 <- readRDS()
ch16 <- readRDS()
ch17 <- readRDS()

klist <- list(k, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17)

tot.withinss <- list()
for (i in 1:17){
  tot.withinss[[i]] <- klist[[i]]$tot.withinss
  
}
plot(4:20, tot.withinss, type = "b", ylab ="tot.withinss", xlab ="Cluster(k)")
tot.withinss

chlist <- list(ch, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9, ch10, ch11, ch12, ch13, ch14, ch15, ch16, ch17)
plot(4:20, chlist, type = "b", ylab ="ch", xlab ="Cluster(k)")
chlist

#####calculo K-means com a base completa e scale igual a T -  Variância 90% #####
####choosing k

setwd()

qtdreg <- 1750
k <- kmeans(data.pca1$x[,1:qtdreg], 4, nstart = 10 )
ch <- calinhara(data.pca1$x[,1:qtdreg], k$cluster)
saveRDS(k, "k.RDS")
saveRDS(ch, "ch.RDS")

k2 <- kmeans(data.pca1$x[,1:qtdreg], 5, nstart = 10 )
ch2 <- calinhara(data.pca1$x[,1:qtdreg], k2$cluster)
saveRDS(k2, "k2.RDS")
saveRDS(ch2, "ch2.RDS")

k3 <- kmeans(data.pca1$x[,1:qtdreg], 6, nstart = 10 )
ch3 <- calinhara(data.pca1$x[,1:qtdreg], k3$cluster)
saveRDS(k3, "k3.RDS")
saveRDS(ch3, "ch3.RDS")

k4 <- kmeans(data.pca1$x[,1:qtdreg], 7, nstart = 10 )
ch4 <- calinhara(data.pca1$x[,1:qtdreg], k4$cluster)
saveRDS(k4, "k4.RDS")
saveRDS(ch4, "ch4.RDS")

k5 <- kmeans(data.pca1$x[,1:qtdreg], 8, nstart = 10)
ch5 <- calinhara(data.pca1$x[,1:qtdreg], k5$cluster)
saveRDS(k5, "k5.RDS")
saveRDS(ch5, "ch5.RDS")

k6 <- kmeans(data.pca1$x[,1:qtdreg], 9, nstart = 10)
ch6 <- calinhara(data.pca1$x[,1:qtdreg], k6$cluster)
saveRDS(k6, "k6.RDS")
saveRDS(ch6, "ch6.RDS")

k7 <- kmeans(data.pca1$x[,1:qtdreg], 10, nstart = 10)
ch7 <- calinhara(data.pca1$x[,1:qtdreg], k7$cluster)
saveRDS(k7, "k7.RDS")
saveRDS(ch7, "ch7.RDS")

k8 <- kmeans(data.pca1$x[,1:qtdreg], 11, nstart = 10)
ch8 <- calinhara(data.pca1$x[,1:qtdreg], k8$cluster)
saveRDS(k8, "k8.RDS")
saveRDS(ch8, "ch8.RDS")

k9 <- kmeans(data.pca1$x[,1:qtdreg], 12, nstart = 10)
ch9 <- calinhara(data.pca1$x[,1:qtdreg], k9$cluster)
saveRDS(k9, "k9.RDS")
saveRDS(ch9, "ch9.RDS")

k10 <- kmeans(data.pca1$x[,1:qtdreg], 13, nstart = 10)
ch10 <- calinhara(data.pca1$x[,1:qtdreg], k10$cluster)
saveRDS(k10, "k10.RDS")
saveRDS(ch10, "ch10.RDS")

k11 <- kmeans(data.pca1$x[,1:qtdreg], 14, nstart = 10)
ch11 <- calinhara(data.pca1$x[,1:qtdreg], k11$cluster)
saveRDS(k11, "k11.RDS")
saveRDS(ch11, "ch11.RDS")

k12 <- kmeans(data.pca1$x[,1:qtdreg], 15, nstart = 10)
ch12 <- calinhara(data.pca1$x[,1:qtdreg], k12$cluster)
saveRDS(k12, "k12.RDS")
saveRDS(ch12, "ch12.RDS")

k13 <- kmeans(data.pca1$x[,1:qtdreg], 16, nstart = 10)
ch13 <- calinhara(data.pca1$x[,1:qtdreg], k13$cluster)
saveRDS(k13, "k13.RDS")
saveRDS(ch13, "ch13.RDS")

k14 <- kmeans(data.pca1$x[,1:qtdreg], 17, nstart = 10)
ch14 <- calinhara(data.pca1$x[,1:qtdreg], k14$cluster)
saveRDS(k14, "k14.RDS")
saveRDS(ch14, "ch14.RDS")

k15 <- kmeans(data.pca1$x[,1:qtdreg], 18, nstart = 10)
ch15 <- calinhara(data.pca1$x[,1:qtdreg], k15$cluster)
saveRDS(k15, "k15.RDS")
saveRDS(ch15, "ch15.RDS")

k16 <- kmeans(data.pca1$x[,1:qtdreg], 19, nstart = 10)
ch16 <- calinhara(data.pca1$x[,1:qtdreg], k16$cluster)
saveRDS(k16, "k16.RDS")
saveRDS(ch16, "ch16.RDS")

k17 <- kmeans(data.pca1$x[,1:qtdreg], 20, nstart = 10)
ch17 <- calinhara(data.pca1$x[,1:qtdreg], k17$cluster)
saveRDS(k17, "k17.RDS")
saveRDS(ch17, "ch17.RDS")

k <- readRDS("k.RDS")
k2 <- readRDS("k2.RDS")
k3 <- readRDS("k3.RDS")
k4 <- readRDS("k4.RDS")
k5 <- readRDS("k5.RDS")
k6 <- readRDS("k6.RDS")
k7 <- readRDS("k7.RDS")
k8 <- readRDS("k8.RDS")
k9 <- readRDS("k9.RDS")
k10 <- readRDS("k10.RDS")
k11 <- readRDS("k11.RDS")
k12 <- readRDS("k12.RDS")
k13 <- readRDS("k13.RDS")
k14 <- readRDS("k14.RDS")
k15 <- readRDS("k15.RDS")
k16 <- readRDS("k16.RDS")
k17 <- readRDS("k17.RDS")

ch <- readRDS("ch.RDS")
ch2 <- readRDS("ch2.RDS")
ch3 <- readRDS("ch3.RDS")
ch4 <- readRDS("ch4.RDS")
ch5 <- readRDS("ch5.RDS")
ch6 <- readRDS("ch6.RDS")
ch7 <- readRDS("ch7.RDS")
ch8 <- readRDS("ch8.RDS")
ch9 <- readRDS("ch9.RDS")
ch10 <- readRDS("ch10.RDS")
ch11 <- readRDS("ch11.RDS")
ch12 <- readRDS("ch12.RDS")
ch13 <- readRDS("ch13.RDS")
ch14 <- readRDS("ch14.RDS")
ch15 <- readRDS("ch15.RDS")
ch16 <- readRDS("ch16.RDS")
ch17 <- readRDS("ch17.RDS")



klist <- list(k, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17)

betweens_totss <- list()
for (i in 1:17){
  betweens_totss[[i]] <- klist[[i]]$betweens/klist[[i]]$totss
  
}
plot(4:20, betweens_totss, type = "b", ylab ="betweenSS/TotalSS", xlab ="Cluster(k)")
betweens_totss

chlist <- list(ch, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9, ch10, ch11, ch12, ch13, ch14, ch15, ch16, ch17)
plot(4:20, chlist, type = "b", ylab ="ch", xlab ="Cluster(k)")


chlist


###calculo K-means com a base completa e scale igual a F
#Variância 85%
####choosing k

k18 <- kmeans(data.pca2$x[,1:1310], 4, nstart = 10 )
ch18 <- calinhara(data.pca2$x[,1:1310], k18$cluster)
saveRDS(k18, "C:\\k18.RDS")
saveRDS(ch18, "C:\\ch18.RDS")

k19 <- kmeans(data.pca2$x[,1:1310], 5, nstart = 10 )
ch19 <- calinhara(data.pca2$x[,1:1310], k19$cluster)
saveRDS(k19, "C:\\k19.RDS")
saveRDS(ch19, "C:\\ch19.RDS")

k20 <- kmeans(data.pca2$x[,1:1310], 6, nstart = 10 )
ch20 <- calinhara(data.pca2$x[,1:1310], k20$cluster)
saveRDS(k20, "C:\\k20.RDS")
saveRDS(ch20, "C:\\ch20.RDS")

k21 <- kmeans(data.pca2$x[,1:1310], 7, nstart = 10 )
ch21 <- calinhara(data.pca2$x[,1:1310], k21$cluster)
saveRDS(k21, "C:\\k21.RDS")
saveRDS(ch21, "C:\\ch21.RDS")

k22 <- kmeans(data.pca2$x[,1:1310], 8, nstart = 10)
ch22 <- calinhara(data.pca2$x[,1:1310], k22$cluster)
saveRDS(k22, "C:\\k22.RDS")
saveRDS(ch22, "C:\\ch22.RDS")

k23 <- kmeans(data.pca2$x[,1:1310], 9, nstart = 10)
ch23 <- calinhara(data.pca2$x[,1:1310], k23$cluster)
saveRDS(k23, "C:\\k23.RDS")
saveRDS(ch23, "C:\\ch23.RDS")

k24 <- kmeans(data.pca2$x[,1:1310], 10, nstart = 10)
ch24 <- calinhara(data.pca2$x[,1:1310], k24$cluster)
saveRDS(k24, "C:\\k24.RDS")
saveRDS(ch24, "C:\\ch24.RDS")

k25 <- kmeans(data.pca2$x[,1:1310], 11, nstart = 10)
ch25 <- calinhara(data.pca2$x[,1:1310], k25$cluster)
saveRDS(k25, "C:\\k25.RDS")
saveRDS(ch25, "C:\\ch25.RDS")

k26 <- kmeans(data.pca2$x[,1:1310], 12, nstart = 10)
ch26 <- calinhara(data.pca2$x[,1:1310], k26$cluster)
saveRDS(k26, "C:\\k26.RDS")
saveRDS(ch26, "C:\\ch26.RDS")

k27 <- kmeans(data.pca2$x[,1:1310], 13, nstart = 10)
ch27 <- calinhara(data.pca2$x[,1:1310], k27$cluster)
saveRDS(k27, "C:\\k27.RDS")
saveRDS(ch27, "C:\\ch27.RDS")

k28 <- kmeans(data.pca2$x[,1:1310], 14, nstart = 10)
ch28 <- calinhara(data.pca2$x[,1:1310], k28$cluster)
saveRDS(k28, "C:\\k28.RDS")
saveRDS(ch28, "C:\\ch28.RDS")

k29 <- kmeans(data.pca2$x[,1:1310], 15, nstart = 10)
ch29 <- calinhara(data.pca2$x[,1:1310], k29$cluster)
saveRDS(k29, "C:\\k29.RDS")
saveRDS(ch29, "C:\\ch29.RDS")

k30 <- kmeans(data.pca2$x[,1:1310], 16, nstart = 10)
ch30 <- calinhara(data.pca2$x[,1:1310], k30$cluster)
saveRDS(k30, "C:\\k30.RDS")
saveRDS(ch30, "C:\\ch30.RDS")

k31 <- kmeans(data.pca2$x[,1:1310], 17, nstart = 10)
ch31 <- calinhara(data.pca2$x[,1:1310], k31$cluster)
saveRDS(k31, "C:\\k31.RDS")
saveRDS(ch31, "C:\\ch31.RDS")

k32 <- kmeans(data.pca2$x[,1:1310], 18, nstart = 10)
ch32 <- calinhara(data.pca2$x[,1:1310], k32$cluster)
saveRDS(k32, "C:\\k32.RDS")
saveRDS(ch32, "C:\\ch32.RDS")

k33 <- kmeans(data.pca2$x[,1:1310], 19, nstart = 10)
ch33 <- calinhara(data.pca2$x[,1:1310], k33$cluster)
saveRDS(k33, "C:\\k33.RDS")
saveRDS(ch33, "C:\\ch33.RDS")

k34 <- kmeans(data.pca2$x[,1:1310], 20, nstart = 10)
ch34 <- calinhara(data.pca2$x[,1:1310], k34$cluster)
saveRDS(k34, "C:\\k34.RDS")
saveRDS(ch34, "C:\\ch34.RDS")

k18 <- readRDS()
k19 <- readRDS()
k20 <- readRDS()
k21 <- readRDS()
k22 <- readRDS()
k23 <- readRDS()
k24 <- readRDS()
k25 <- readRDS()
k26 <- readRDS()
k27 <- readRDS()
k28 <- readRDS()
k29 <- readRDS()
k30 <- readRDS()
k31 <- readRDS()
k32 <- readRDS()
k33 <- readRDS()
k34 <- readRDS()

ch18 <- readRDS()
ch19 <- readRDS()
ch20 <- readRDS()
ch21 <- readRDS()
ch22 <- readRDS()
ch23 <- readRDS()
ch24 <- readRDS()
ch25 <- readRDS()
ch26 <- readRDS()
ch27 <- readRDS()
ch28 <- readRDS()
ch29 <- readRDS()
ch30 <- readRDS()
ch31 <- readRDS()
ch32 <- readRDS()
ch33 <- readRDS()
ch34 <- readRDS()

klist2 <- list(k18, k19, k20, k21, k22, k23, k24, k25, k26, k27, k28, k29, k30, k31, k32, k33, k34)

tot.withinss2 <- list()
for (i in 1:17){
  tot.withinss2[[i]] <- klist2[[i]]$tot.withinss
  
}
plot(4:20, tot.withinss2, type = "b", ylab ="tot.withinss", xlab ="Cluster(k)")
tot.withinss2

chlist2 <- list(ch18, ch19, ch20, ch21, ch22, ch23, ch24, ch25, ch26, ch27, ch28, ch29, ch30, ch31, ch32, ch33, ch34)
plot(4:20, chlist2, type = "b", ylab ="ch", xlab ="Cluster(k)")
chlist2

#####calculo K-means com a base completa e scale igual a F -  Variância 90% #####
####choosing k

setwd()


#redução de dimensionalidade da feature
#data.pca1 <- prcomp(feature[,1:2181], scale = F)
#saveRDS(data.pca1, "pca1sF.RDS")
data.pca1 <- readRDS("pca1sF.RDS")

#data.pca2 <- prcomp(feature[,1:2181], scale = F)
#saveRDS(data.pca2, "pca2sF.RDS")
data.pca2 <- readRDS("pca2sF.RDS")


qtdreg <- 1520
ksF <- kmeans(data.pca1$x[,1:qtdreg], 4, nstart = 10 )
chsF <- calinhara(data.pca1$x[,1:qtdreg], ksF$cluster)
saveRDS(ksF, "ksF.RDS")
saveRDS(chsF, "chsF.RDS")

k2sF <- kmeans(data.pca1$x[,1:qtdreg], 5, nstart = 10 )
ch2sF <- calinhara(data.pca1$x[,1:qtdreg], k2sF$cluster)
saveRDS(k2sF, "k2sF.RDS")
saveRDS(ch2sF, "ch2sF.RDS")

k3sF <- kmeans(data.pca1$x[,1:qtdreg], 6, nstart = 10 )
ch3sF <- calinhara(data.pca1$x[,1:qtdreg], k3sF$cluster)
saveRDS(k3sF, "k3sF.RDS")
saveRDS(ch3sF, "ch3sF.RDS")

k4sF <- kmeans(data.pca1$x[,1:qtdreg], 7, nstart = 10 )
ch4sF <- calinhara(data.pca1$x[,1:qtdreg], k4sF$cluster)
saveRDS(k4sF, "k4sF.RDS")
saveRDS(ch4sF, "ch4sF.RDS")

k5sF <- kmeans(data.pca1$x[,1:qtdreg], 8, nstart = 10)
ch5sF <- calinhara(data.pca1$x[,1:qtdreg], k5sF$cluster)
saveRDS(k5sF, "k5sF.RDS")
saveRDS(ch5sF, "ch5sF.RDS")

k6sF <- kmeans(data.pca1$x[,1:qtdreg], 9, nstart = 10)
ch6sF <- calinhara(data.pca1$x[,1:qtdreg], k6sF$cluster)
saveRDS(k6sF, "k6sF.RDS")
saveRDS(ch6sF, "ch6sF.RDS")

k7sF <- kmeans(data.pca1$x[,1:qtdreg], 10, nstart = 10)
ch7sF <- calinhara(data.pca1$x[,1:qtdreg], k7sF$cluster)
saveRDS(k7sF, "k7sF.RDS")
saveRDS(ch7sF, "ch7sF.RDS")

k8sF <- kmeans(data.pca1$x[,1:qtdreg], 11, nstart = 10)
ch8sF <- calinhara(data.pca1$x[,1:qtdreg], k8sF$cluster)
saveRDS(k8sF, "k8sF.RDS")
saveRDS(ch8sF, "ch8sF.RDS")

k9sF <- kmeans(data.pca1$x[,1:qtdreg], 12, nstart = 10)
ch9sF <- calinhara(data.pca1$x[,1:qtdreg], k9sF$cluster)
saveRDS(k9sF, "k9sF.RDS")
saveRDS(ch9sF, "ch9sF.RDS")

k10sF <- kmeans(data.pca1$x[,1:qtdreg], 13, nstart = 10)
ch10sF <- calinhara(data.pca1$x[,1:qtdreg], k10sF$cluster)
saveRDS(k10sF, "k10sF.RDS")
saveRDS(ch10sF, "ch10sF.RDS")

k11sF <- kmeans(data.pca1$x[,1:qtdreg], 14, nstart = 10)
ch11sF <- calinhara(data.pca1$x[,1:qtdreg], k11sF$cluster)
saveRDS(k11sF, "k11sF.RDS")
saveRDS(ch11sF, "ch11sF.RDS")

k12sF <- kmeans(data.pca1$x[,1:qtdreg], 15, nstart = 10)
ch12sF <- calinhara(data.pca1$x[,1:qtdreg], k12sF$cluster)
saveRDS(k12sF, "k12sF.RDS")
saveRDS(ch12sF, "ch12sF.RDS")

k13sF <- kmeans(data.pca1$x[,1:qtdreg], 16, nstart = 10)
ch13sF <- calinhara(data.pca1$x[,1:qtdreg], k13sF$cluster)
saveRDS(k13sF, "k13sF.RDS")
saveRDS(ch13sF, "ch13sF.RDS")

k14sF <- kmeans(data.pca1$x[,1:qtdreg], 17, nstart = 10)
ch14sF <- calinhara(data.pca1$x[,1:qtdreg], k14sF$cluster)
saveRDS(k14sF, "k14sF.RDS")
saveRDS(ch14sF, "ch14sF.RDS")

k15sF <- kmeans(data.pca1$x[,1:qtdreg], 18, nstart = 10)
ch15sF <- calinhara(data.pca1$x[,1:qtdreg], k15sF$cluster)
saveRDS(k15sF, "k15sF.RDS")
saveRDS(ch15sF, "ch15sF.RDS")

k16sF <- kmeans(data.pca1$x[,1:qtdreg], 19, nstart = 10)
ch16sF <- calinhara(data.pca1$x[,1:qtdreg], k16sF$cluster)
saveRDS(k16sF, "k16sF.RDS")
saveRDS(ch16sF, "ch16sF.RDS")

k17sF <- kmeans(data.pca1$x[,1:qtdreg], 20, nstart = 10)
ch17sF <- calinhara(data.pca1$x[,1:qtdreg], k17sF$cluster)
saveRDS(k17sF, "k17sF.RDS")
saveRDS(ch17sF, "ch17sF.RDS")

ksF <- readRDS("ksF.RDS")
k2sF <- readRDS("k2sF.RDS")
k3sF <- readRDS("k3sF.RDS")
k4sF <- readRDS("k4sF.RDS")
k5sF <- readRDS("k5sF.RDS")
k6sF <- readRDS("k6sF.RDS")
k7sF <- readRDS("k7sF.RDS")
k8sF <- readRDS("k8sF.RDS")
k9sF <- readRDS("k9sF.RDS")
k10sF <- readRDS("k10sF.RDS")
k11sF <- readRDS("k11sF.RDS")
k12sF <- readRDS("k12sF.RDS")
k13sF <- readRDS("k13sF.RDS")
k14sF <- readRDS("k14sF.RDS")
k15sF <- readRDS("k15sF.RDS")
k16sF <- readRDS("k16sF.RDS")
k17sF <- readRDS("k17sF.RDS")


chsF <- readRDS("chsF.RDS")
ch2sF <- readRDS("ch2sF.RDS")
ch3sF <- readRDS("ch3sF.RDS")
ch4sF <- readRDS("ch4sF.RDS")
ch5sF <- readRDS("ch5sF.RDS")
ch6sF <- readRDS("ch6sF.RDS")
ch7sF <- readRDS("ch7sF.RDS")
ch8sF <- readRDS("ch8sF.RDS")
ch9sF <- readRDS("ch9sF.RDS")
ch10sF <- readRDS("ch10sF.RDS")
ch11sF <- readRDS("ch11sF.RDS")
ch12sF <- readRDS("ch12sF.RDS")
ch13sF <- readRDS("ch13sF.RDS")
ch14sF <- readRDS("ch14sF.RDS")
ch15sF <- readRDS("ch15sF.RDS")
ch16sF <- readRDS("ch16sF.RDS")
ch17sF <- readRDS("ch17sF.RDS")


listsF <- list(ksF, k2sF, k3sF, k4sF, k5sF, k6sF, k7sF, k8sF, k9sF, k10sF, k11sF, k12sF, k13sF, k14sF, k15sF, k16sF, k17sF)

betweens_totss <- list()
for (i in 1:17){
  betweens_totss[[i]] <- klistsF[[i]]$betweens/klistsF[[i]]$totss
  
}
plot(4:20, betweens_totss, type = "b", ylab ="betweenSS/TotalSS", xlab ="Cluster(k)")
betweens_totss

chlistsF <- list(chsF, ch2sF, ch3sF, ch4sF, ch5sF, ch6sF, ch7sF, ch8sF, ch9sF, ch10sF, ch11sF, ch12sF, ch13sF, ch14sF, ch15sF, ch16sF, ch17sF)
plot(4:20, chlistsF, type = "b", ylab ="ch", xlab ="Cluster(k)")

chlistsF


#########
#k medians
clmedians=kcca(data.pca2$x[,1:1310], k = 4, family=kccaFamily("kmedians"))
ch35 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians))
saveRDS(clmedians, "C:\\clmedians.RDS")
saveRDS(ch35, "C:\\ch35.RDS")

clmedians2=kcca(data.pca2$x[,1:1310], k = 5, family=kccaFamily("kmedians"))
ch36 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians2))
saveRDS(clmedians2, "C:\\clmedians2.RDS")
saveRDS(ch36, "C:\\ch36.RDS")

clmedians3=kcca(data.pca2$x[,1:1310], k = 6, family=kccaFamily("kmedians"))
ch37 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians3))
saveRDS(clmedians3, "C:\\clmedians3.RDS")
saveRDS(ch37, "C:\\ch37.RDS")

clmedians4=kcca(data.pca2$x[,1:1310], k = 7, family=kccaFamily("kmedians"))
ch38 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians4))
saveRDS(clmedians4, "C:\\clmedians4.RDS")
saveRDS(ch38, "C:\\ch38.RDS")

clmedians5=kcca(data.pca2$x[,1:1310], k = 8, family=kccaFamily("kmedians"))
ch39 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians5))
saveRDS(clmedians5, "C:\\clmedians5.RDS")
saveRDS(ch39, "C:\\ch39.RDS")

clmedians6=kcca(data.pca2$x[,1:1310], k = 9, family=kccaFamily("kmedians"))
ch40 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians6))
saveRDS(clmedians6, "C:\\clmedians6.RDS")
saveRDS(ch40, "C:\\ch40.RDS")

clmedians7=kcca(data.pca2$x[,1:1310], k = 10, family=kccaFamily("kmedians"))
ch41 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians7))
saveRDS(clmedians7, "C:\\clmedians7.RDS")
saveRDS(ch41, "C:\\ch41.RDS")

clmedians8=kcca(data.pca2$x[,1:1310], k = 11, family=kccaFamily("kmedians"))
ch42 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians8))
saveRDS(clmedians8, "C:\\clmedians8.RDS")
saveRDS(ch42, "C:\\ch42.RDS")

clmedians9=kcca(data.pca2$x[,1:1310], k = 12, family=kccaFamily("kmedians"))
ch43 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians9))
saveRDS(clmedians9, "C:\\clmedians9.RDS")
saveRDS(ch43, "C:\\ch43.RDS")

clmedians10=kcca(data.pca2$x[,1:1310], k = 13, family=kccaFamily("kmedians"))
ch44 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians10))
saveRDS(clmedians10, "C:\\clmedians10.RDS")
saveRDS(ch44, "C:\\ch44.RDS")

clmedians11=kcca(data.pca2$x[,1:1310], k = 14, family=kccaFamily("kmedians"))
ch45 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians11))
saveRDS(clmedians11, "C:\\clmedians11.RDS")
saveRDS(ch45, "C:\\ch45.RDS")

clmedians12=kcca(data.pca2$x[,1:1310], k = 15, family=kccaFamily("kmedians"))
ch46 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians12))
saveRDS(clmedians12, "C:\\clmedians12.RDS")
saveRDS(ch46, "C:\\ch46.RDS")

clmedians13=kcca(data.pca2$x[,1:1310], k = 16, family=kccaFamily("kmedians"))
ch47 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians13))
saveRDS(clmedians13, "C:\\clmedians13.RDS")
saveRDS(ch47, "C:\\ch47.RDS")

clmedians14=kcca(data.pca2$x[,1:1310], k = 17, family=kccaFamily("kmedians"))
ch48 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians14))
saveRDS(clmedians14, "C:\\clmedians14.RDS")
saveRDS(ch48, "C:\\ch48.RDS")

clmedians15=kcca(data.pca2$x[,1:1310], k = 18, family=kccaFamily("kmedians"))
ch49 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians15))
saveRDS(clmedians15, "C:\\clmedians15.RDS")
saveRDS(ch49, "C:\\ch49.RDS")

clmedians16=kcca(data.pca2$x[,1:1310], k = 19, family=kccaFamily("kmedians"))
ch50 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians16))
saveRDS(clmedians16, "C:\\clmedians16.RDS")
saveRDS(ch50, "C:\\ch50.RDS")

clmedians17=kcca(data.pca2$x[,1:1310], k = 20, family=kccaFamily("kmedians"))
ch51 <- calinhara(data.pca2$x[,1:1310], clusters(clmedians17))
saveRDS(clmedians17, "C:\\clmedians17.RDS")
saveRDS(ch51, "C:\\ch51.RDS")

clmedians <- readRDS("C:\\clmedians.RDS")
clmedians2 <- readRDS("C:\\clmedians2.RDS")
clmedians3 <- readRDS("C:\\clmedians3.RDS")
clmedians4 <- readRDS("C:\\clmedians4.RDS")
clmedians5 <- readRDS("C:\\clmedians5.RDS")
clmedians6 <- readRDS("C:\\clmedians6.RDS")
clmedians7 <- readRDS("C:\\clmedians7.RDS")
clmedians8 <- readRDS("C:\\clmedians8.RDS")
clmedians9 <- readRDS("C:\\clmedians9.RDS")
clmedians10 <- readRDS("C:\\clmedians10.RDS")
clmedians11 <- readRDS("C:\\clmedians11.RDS")
clmedians12 <- readRDS("C:\\clmedians12.RDS")
clmedians13 <- readRDS("C:\\clmedians13.RDS")
clmedians14 <- readRDS("C:\\clmedians14.RDS")
clmedians15 <- readRDS("C:\\clmedians15.RDS")
clmedians16 <- readRDS("C:\\clmedians16.RDS")
clmedians17 <- readRDS("C:\\clmedians17.RDS")

ch35 <- readRDS("C:\\ch35.RDS")
ch36 <- readRDS("C:\\ch36.RDS")
ch37 <- readRDS("C:\\ch37.RDS")
ch38 <- readRDS("C:\\ch38.RDS")
ch39 <- readRDS("C:\\ch39.RDS")
ch40 <- readRDS("C:\\ch40.RDS")
ch41 <- readRDS("C:\\ch41.RDS")
ch42 <- readRDS("C:\\ch42.RDS")
ch43 <- readRDS("C:\\ch43.RDS")
ch44 <- readRDS("C:\\ch44.RDS")
ch45 <- readRDS("C:\\ch45.RDS")
ch46 <- readRDS("C:\\ch46.RDS")
ch47 <- readRDS("C:\\ch47.RDS")
ch48 <- readRDS("C:\\ch48.RDS")
ch49 <- readRDS("C:\\ch49.RDS")
ch50 <- readRDS("C:\\ch50.RDS")
ch51 <- readRDS("C:\\ch51.RDS")


clmediansList <- list(clmedians, clmedians2, clmedians3, clmedians4, clmedians5, clmedians6, clmedians7, clmedians8, clmedians9, clmedians10, clmedians11, clmedians12, clmedians13, clmedians14, clmedians15, clmedians16, clmedians17)

tot.withinss3 <- list()
for (i in 1:17){
  tot.withinss3[[i]] <- sum(rowSums((data.pca2$x[,1:1310] - clmediansList[[i]]@centers[clmediansList[[i]]@cluster,])^2))
  
}
plot(4:20, tot.withinss3, type = "b", ylab ="tot.withinss", xlab ="Cluster(k)")
tot.withinss3

chlist3 <- list(ch35, ch36, ch37, ch38, ch39, ch40, ch41, ch42, ch43, ch44, ch45, ch46, ch47, ch48, ch49, ch50, ch51)
plot(4:20, chlist3, type = "b", ylab ="ch", xlab ="Cluster(k)")
chlist3


##############k fuzzy c-means
clcmeans=cmeans(data.pca2$x[,1:1310],4, m = 2)
ch52 <- calinhara(data.pca2$x[,1:1310], clcmeans$cluster)
saveRDS(clcmeans, "C:\\clcmeans.RDS")
saveRDS(ch52, "C:\\ch52.RDS")

clcmeans2=cmeans(data.pca2$x[,1:1310],5, m = 2)
ch53 <- calinhara(data.pca2$x[,1:1310], clcmeans2$cluster)
saveRDS(clcmeans2, "C:\\clcmeans2.RDS")
saveRDS(ch53, "C:\\ch53.RDS")

clcmeans3=cmeans(data.pca2$x[,1:1310],6, m = 2)
ch54 <- calinhara(data.pca2$x[,1:1310], clcmeans3$cluster)
saveRDS(clcmeans3, "C:\\clcmeans3.RDS")
saveRDS(ch54, "C:\\ch54.RDS")

clcmeans4=cmeans(data.pca2$x[,1:1310],7, m = 2)
ch55 <- calinhara(data.pca2$x[,1:1310], clcmeans4$cluster)
saveRDS(clcmeans4, "C:\\clcmeans4.RDS")
saveRDS(ch55, "C:\\ch55.RDS")

clcmeans5=cmeans(data.pca2$x[,1:1310],8, m = 2)
ch56 <- calinhara(data.pca2$x[,1:1310], clcmeans5$cluster)
saveRDS(clcmeans5, "C:\\clcmeans5.RDS")
saveRDS(ch56, "C:\\ch56.RDS")

clcmeans6=cmeans(data.pca2$x[,1:1310],9, m = 2)
ch57 <- calinhara(data.pca2$x[,1:1310], clcmeans6$cluster)
saveRDS(clcmeans6, "C:\\clcmeans6.RDS")
saveRDS(ch57, "C:\\ch57.RDS")

clcmeans7=cmeans(data.pca2$x[,1:1310],10, m = 2)
ch58 <- calinhara(data.pca2$x[,1:1310], clcmeans7$cluster)
saveRDS(clcmeans7, "C:\\clcmeans7.RDS")
saveRDS(ch58, "C:\\ch58.RDS")

clcmeans8=cmeans(data.pca2$x[,1:1310],11, m = 2)
ch59 <- calinhara(data.pca2$x[,1:1310], clcmeans8$cluster)
saveRDS(clcmeans8, "C:\\clcmeans8.RDS")
saveRDS(ch59, "C:\\ch59.RDS")

clcmeans9=cmeans(data.pca2$x[,1:1310],12, m = 2)
ch60 <- calinhara(data.pca2$x[,1:1310], clcmeans9$cluster)
saveRDS(clcmeans9, "C:\\clcmeans9.RDS")
saveRDS(ch60, "C:\\ch60.RDS")

clcmeans10=cmeans(data.pca2$x[,1:1310],13, m = 2)
ch61 <- calinhara(data.pca2$x[,1:1310], clcmeans10$cluster)
saveRDS(clcmeans10, "C:\\clcmeans10.RDS")
saveRDS(ch61, "C:\\ch61.RDS")

clcmeans11=cmeans(data.pca2$x[,1:1310],14, m = 2)
ch62 <- calinhara(data.pca2$x[,1:1310], clcmeans11$cluster)
saveRDS(clcmeans11, "C:\\clcmeans11.RDS")
saveRDS(ch62, "C:\\ch62.RDS")

clcmeans12=cmeans(data.pca2$x[,1:1310],15, m = 2)
ch63 <- calinhara(data.pca2$x[,1:1310], clcmeans12$cluster)
saveRDS(clcmeans12, "C:\\clcmeans12.RDS")
saveRDS(ch63, "C:\\ch63.RDS")

clcmeans13=cmeans(data.pca2$x[,1:1310],16, m = 2)
ch64 <- calinhara(data.pca2$x[,1:1310], clcmeans13$cluster)
saveRDS(clcmeans13, "C:\\clcmeans13.RDS")
saveRDS(ch64, "C:\\ch64.RDS")

clcmeans14=cmeans(data.pca2$x[,1:1310],17, m = 2)
ch65 <- calinhara(data.pca2$x[,1:1310], clcmeans14$cluster)
saveRDS(clcmeans14, "C:\\clcmeans14.RDS")
saveRDS(ch65, "C:\\ch65.RDS")

clcmeans15=cmeans(data.pca2$x[,1:1310],18, m = 2)
ch66 <- calinhara(data.pca2$x[,1:1310], clcmeans15$cluster)
saveRDS(clcmeans15, "C:\\clcmeans15.RDS")
saveRDS(ch66, "C:\\ch66.RDS")

clcmeans16=cmeans(data.pca2$x[,1:1310],19, m = 2)
ch67 <- calinhara(data.pca2$x[,1:1310], clcmeans16$cluster)
saveRDS(clcmeans16, "C:\\clcmeans16.RDS")
saveRDS(ch67, "C:\ch67.RDS")

clcmeans17=cmeans(data.pca2$x[,1:1310],20, m = 2)
ch68 <- calinhara(data.pca2$x[,1:1310], clcmeans17$cluster)
saveRDS(clcmeans17, "C:\\clcmeans17.RDS")
saveRDS(ch68, "C:\\ch68.RDS")

clcmeans <- readRDS("C:\\clcmeans.RDS")
clcmeans2 <- readRDS("C:\\clcmeans2.RDS")
clcmeans3 <- readRDS("C:\\clcmeans3.RDS")
clcmeans4 <- readRDS("C:\\clcmeans4.RDS")
clcmeans5 <- readRDS("C:\\clcmeans5.RDS")
clcmeans6 <- readRDS("C:\\clcmeans6.RDS")
clcmeans7 <- readRDS("C:\\clcmeans7.RDS")
clcmeans8 <- readRDS("C:\\clcmeans8.RDS")
clcmeans9 <- readRDS("C:\\clcmeans9.RDS")
clcmeans10 <- readRDS("C:\\clcmeans10.RDS")
clcmeans11 <- readRDS("C:\\clcmeans11.RDS")
clcmeans12 <- readRDS("C:\\clcmeans12.RDS")
clcmeans13 <- readRDS("C:\\clcmeans13.RDS")
clcmeans14 <- readRDS("C:\\clcmeans14.RDS")
clcmeans15 <- readRDS("C:\\clcmeans15.RDS")
clcmeans16 <- readRDS("C:\\clcmeans16.RDS")
clcmeans17 <- readRDS("C:\\clcmeans17.RDS")

ch52 <- readRDS("C:\\ch52.RDS")
ch53 <- readRDS("C:\\ch53.RDS")
ch54 <- readRDS("C:\\ch54.RDS")
ch55 <- readRDS("C:\\ch55.RDS")
ch56 <- readRDS("C:\\ch56.RDS")
ch57 <- readRDS("C:\\ch57.RDS")
ch58 <- readRDS("C:\\ch58.RDS")
ch59 <- readRDS("C:\\ch59.RDS")
ch60 <- readRDS("C:\\ch60.RDS")
ch61 <- readRDS("C:\\ch61.RDS")
ch62 <- readRDS("C:\\ch62.RDS")
ch63 <- readRDS("C:\\ch63.RDS")
ch64 <- readRDS("C:\\ch64.RDS")
ch65<- readRDS("C:\\ch65.RDS")
ch66 <- readRDS("C:\\ch66.RDS")
ch67 <- readRDS("C:\\ch67.RDS")
ch68 <- readRDS("C:\\ch68.RDS")

clcmeansList <- list(clcmeans, clcmeans2, clcmeans3, clcmeans4, clcmeans5, clcmeans6, clcmeans7, clcmeans8, clcmeans9, clcmeans10, clcmeans11, clcmeans12, clcmeans13, clcmeans14, clcmeans15, clcmeans16, clcmeans17)

tot.withinss4 <- list()
for (i in 1:17){
  tot.withinss4[[i]] <- sum(rowSums((data.pca2$x[,1:1310] - clcmeansList[[i]]$centers[clcmeansList[[i]]$cluster, ])^2))
  
}
plot(4:20, tot.withinss4, type = "b", ylab ="tot.withinss", xlab ="Cluster(k)")
tot.withinss4

chlist4 <- list(ch52, ch53, ch54, ch55, ch56, ch57, ch58, ch59, ch60, ch61, ch62, ch63, ch64, ch65, ch66, ch67, ch68)
plot(4:20, chlist4, type = "b", ylab ="ch", xlab ="Cluster(k)")
chlist4

####iniciando o melhor modelo de cluster######
kb18 <- kmeans(data.pca2$x[,1:1310], 4, nstart = 50 )
chb18 <- calinhara(data.pca2$x[,1:1310], kb18$cluster)
saveRDS(kb18, "C:\\kb18.RDS")
saveRDS(chb18, "C:\\chb18.RDS")
kb18$tot.withinss
kb18 <- readRDS("C:\\kb18.RDS")
kb18$size


#############Análise Bigramas############
#Adicionar no headline os grupos cluster e o split do headline_text
headline$cluster <- kb18$cluster
headline$split <- strsplit(as.character(headline$headline_text), " ", fixed = TRUE)
head(headline)

###library stopwords
library(tm)

#filtrar um cluster para analisar o bigrama
cluster <- headline[headline$cluster==1, ]
head(cluster)
unico <- c()
for (i in 1:nrow(cluster)){
  #head <- unlist(headline$split[i],paste)
  head <- ngrams(unlist(cluster$split[i],paste),2)
  v <- vapply(head, paste, "", collapse = " ")
  unico <- c(unico,v)
}
unico


###removendo stopwords
documents <- Corpus(VectorSource(unico))
documents = tm_map(documents, removeWords, stopwords("english"))
documents$content
count3 <- as.data.frame(table(documents$content));count3
count4 <- count3[order(count3$Freq,decreasing = T),];count4



####fitro notícias de 2016

headline$datapublicacao  <- as.POSIXct(strptime(headline[,1], "%Y%m%d"))
head(headline)

headline2016 <- headline[headline[ ,5] >= "2016-01-01"
                         & headline[ ,5] <= "2016-12-31" & !is.na(headline[ ,5]) , ]  
head(headline2016)
nrow(headline2016)

#feature2016 <- feature[headline2016[ ,1] , ]
feature2016 <- data.pca2$x[as.numeric(row.names(headline2016)),1:1310]
head(feature2016)
nrow(feature2016)

####cluster noticias 2016######

km <- kmeans(feature2016, 4, nstart = 10 )
chm <- calinhara(feature2016, km$cluster)
saveRDS(km, "C:\\km.RDS")
saveRDS(chm, "C:\\chm.RDS")

km2 <- kmeans(feature2016, 5, nstart = 10 )
chm2 <- calinhara(feature2016, km2$cluster)
saveRDS(km2, "C:\\km2.RDS")
saveRDS(chm2, "C:\\chm2.RDS")

km3 <- kmeans(feature2016, 6, nstart = 10 )
chm3 <- calinhara(feature2016, km3$cluster)
saveRDS(km3, "C:\\km3.RDS")
saveRDS(chm3, "C:\\chm3.RDS")

km4 <- kmeans(feature2016, 7, nstart = 10 )
chm4 <- calinhara(feature2016, km4$cluster)
saveRDS(km4, "C:\\km4.RDS")
saveRDS(chm4, "C:\\chm4.RDS")

km5 <- kmeans(feature2016, 8, nstart = 10 )
chm5 <- calinhara(feature2016, km5$cluster)
saveRDS(km5, "C:\\km5.RDS")
saveRDS(chm5, "C:\\chm5.RDS")

km6 <- kmeans(feature2016, 9, nstart = 10 )
chm6 <- calinhara(feature2016, km6$cluster)
saveRDS(km6, "C:\\km6.RDS")
saveRDS(chm6, "C:\\chm6.RDS")

km7 <- kmeans(feature2016, 10, nstart = 10 )
chm7 <- calinhara(feature2016, km7$cluster)
saveRDS(km7, "C:\\km7.RDS")
saveRDS(chm7, "C:\\chm7.RDS")

km8 <- kmeans(feature2016, 11, nstart = 10 )
chm8 <- calinhara(feature2016, km8$cluster)
saveRDS(km8, "C:\\km8.RDS")
saveRDS(chm8, "C:\\chm8.RDS")

km9 <- kmeans(feature2016, 12, nstart = 10 )
chm9 <- calinhara(feature2016, km9$cluster)
saveRDS(km9, "C:\\km9.RDS")
saveRDS(chm9, "C:\\chm9.RDS")

km10 <- kmeans(feature2016, 13, nstart = 10 )
chm10 <- calinhara(feature2016, km10$cluster)
saveRDS(km10, "C:\\km10.RDS")
saveRDS(chm10, "C:\\chm10.RDS")

km11 <- kmeans(feature2016, 14, nstart = 10 )
chm11 <- calinhara(feature2016, km11$cluster)
saveRDS(km11, "C:\\km11.RDS")
saveRDS(chm11, "C:\\chm11.RDS")

km12 <- kmeans(feature2016, 15, nstart = 10 )
chm12 <- calinhara(feature2016, km12$cluster)
saveRDS(km12, "C:\\km12.RDS")
saveRDS(chm12, "C:\\chm12.RDS")

km13 <- kmeans(feature2016, 16, nstart = 10 )
chm13 <- calinhara(feature2016, km13$cluster)
saveRDS(km13, "C:\\km13.RDS")
saveRDS(chm13, "C:\\chm13.RDS")

km14 <- kmeans(feature2016, 17, nstart = 10 )
chm14 <- calinhara(feature2016, km14$cluster)
saveRDS(km14, "C:\\km14.RDS")
saveRDS(chm14, "C:\\chm14.RDS")

km15 <- kmeans(feature2016, 18, nstart = 10 )
chm15 <- calinhara(feature2016, km15$cluster)
saveRDS(km15, "C:\\km15.RDS")
saveRDS(chm15, "C:\\chm15.RDS")

km16 <- kmeans(feature2016, 19, nstart = 10 )
chm16 <- calinhara(feature2016, km16$cluster)
saveRDS(km16, "C:\\km16.RDS")
saveRDS(chm16, "C:\\chm16.RDS")

km17 <- kmeans(feature2016, 20, nstart = 10 )
chm17 <- calinhara(feature2016, km17$cluster)
saveRDS(km17, "C:\\km17.RDS")
saveRDS(chm17, "C:\\chm17.RDS")

km <- readRDS("C:\\km.RDS")
km2 <- readRDS("C:\\km2.RDS")
km3 <- readRDS("C:\\km3.RDS")
km4 <- readRDS("C:\\km4.RDS")
km5 <- readRDS("C:\\km5.RDS")
km6 <- readRDS("C:\\km6.RDS")
km7 <- readRDS("C:\\km7.RDS")
km8 <- readRDS("C:\\km8.RDS")
km9 <- readRDS("C:\\km9.RDS")
km10 <- readRDS("C:\\km10.RDS")
km11 <- readRDS("C:\\km11.RDS")
km12 <- readRDS("C:\\km12.RDS")
km13 <- readRDS("C:\\km13.RDS")
km14 <- readRDS("C:\\km14.RDS")
km15 <- readRDS("C:\\km15.RDS")
km16 <- readRDS("C:\\km16.RDS")
km17 <- readRDS("C:\\km17.RDS")

chm <- readRDS("C:\\chm.RDS")
chm2 <- readRDS("C:\\chm2.RDS")
chm3 <- readRDS("C:\\chm3.RDS")
chm4 <- readRDS("C:\\chm4.RDS")
chm5 <- readRDS("C:\\chm5.RDS")
chm6 <- readRDS("C:\\chm6.RDS")
chm7 <- readRDS("C:\\chm7.RDS")
chm8 <- readRDS("C:\\chm8.RDS")
chm9 <- readRDS("C:\\chm9.RDS")
chm10 <- readRDS("C:\\chm10.RDS")
chm11 <- readRDS("C:\\chm11.RDS")
chm12 <- readRDS("C:\\chm12.RDS")
chm13 <- readRDS("C:\\chm13.RDS")
chm14 <- readRDS("C:\\chm14.RDS")
chm15 <- readRDS("C:\\chm15.RDS")
chm16 <- readRDS("C:\\chm16.RDS")
chm17 <- readRDS("C:\\chm17.RDS")

klist2016 <- list(km, km2, km3, km4, km5, km6, km7, km8, km9, km10, km11, km12, km13, km14, km15, km16, km17)

tot.withinss2016 <- list()
for (i in 1:17){
  tot.withinss2016[[i]] <- klist2016[[i]]$tot.withinss
  
}
plot(4:20, tot.withinss2016, type = "b", ylab ="tot.withinss", xlab ="Cluster(k)")
tot.withinss2016

chlist2016 <- list(chm, chm2, chm3, chm4, chm5, chm6, chm7, chm8, chm9, chm10, chm11, chm12, chm13, chm14, chm15, chm16, chm17)
plot(4:20, chlist2016, type = "b", ylab ="ch", xlab ="Cluster(k)")
chlist2016

km$size
#filtrar um cluster para analisar o bigrama, k = 4
headline2016$cluster <- km$cluster
cluster <- headline2016[headline2016$cluster==4, ]
head(cluster)
unico <- c()
for (i in 1:nrow(cluster)){
  #head <- unlist(headline$split[i],paste)
  head <- ngrams(unlist(cluster$split[i],paste),2)
  v <- vapply(head, paste, "", collapse = " ")
  unico <- c(unico,v)
}
unico


###removendo stopwords
documents <- Corpus(VectorSource(unico))
documents = tm_map(documents, removeWords, stopwords("english"))
documents$content
count3 <- as.data.frame(table(documents$content));count3
count4 <- count3[order(count3$Freq,decreasing = T),];count4

#filtrar um cluster para analisar o bigrama, k = 7
headline2016$cluster <- km4$cluster
cluster <- headline2016[headline2016$cluster==7, ]
head(cluster)
unico <- c()
for (i in 1:nrow(cluster)){
  #head <- unlist(headline$split[i],paste)
  head <- ngrams(unlist(cluster$split[i],paste),2)
  v <- vapply(head, paste, "", collapse = " ")
  unico <- c(unico,v)
}
unico


###removendo stopwords
documents <- Corpus(VectorSource(unico))
documents = tm_map(documents, removeWords, stopwords("english"))
documents$content
count3 <- as.data.frame(table(documents$content));count3
count4 <- count3[order(count3$Freq,decreasing = T),];count4

#####melhorando o resultado do k-means#####
cluster <- headline[headline$cluster==1, ];cluster
featureN <- data.pca2$x[as.numeric(row.names(cluster)),1:1310]
nrow(featureN)
knew <- kmeans(featureN, 2, nstart = 1 )
chnew <- calinhara(featureN, knew$cluster)
knew$size

cluster$cluster2 <- knew$cluster
head(cluster)

cluster2 <- cluster[cluster$cluster2==2, ];cluster2

unico <- c()
for (i in 1:nrow(cluster2)){
  #head <- unlist(headline$split[i],paste)
  head <- ngrams(unlist(cluster2$split[i],paste),2)
  v <- vapply(head, paste, "", collapse = " ")
  unico <- c(unico,v)
}
unico


###removendo stopwords
documents <- Corpus(VectorSource(unico))
documents = tm_map(documents, removeWords, stopwords("english"))
documents$content
count3 <- as.data.frame(table(documents$content));count3
count4 <- count3[order(count3$Freq,decreasing = T),];count4