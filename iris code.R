library(cluster)
library(mclust)
library(clusterCrit)
library(randomForest)
library(aricode)
#library(GIC)
library(Rcpp)
library(RcppHungarian)

#source("H:\\apps\\xp\\desktop\\R Code\\New Clustering 2\\Final_Code_prob.R")

data(iris)
#
d=iris
#d=read.csv("H:\\apps\\xp\\desktop\\R Code\\Clustering method\\Paper purpose\\iris\\iris.csv")
d$ct=ifelse(d$Species=="setosa",1,ifelse(d$Species=="versicolor",2,3))
d=d[,-5]
ct=d$ct
d=d[,-5]

###New method equation 3

start_time <- Sys.time()
a1=prox2(d,4)
end_time <- Sys.time()

end_time-start_time

D1 = sqrt(1-a1)
pamRF1 <- pam(D1, k = 3)
sil1=summary(silhouette(pamRF1$clustering,D1))
sil1$avg.width

adjustedRandIndex(pamRF1$clustering,ct)
NMI(pamRF1$clustering,ct)
u=table(pamRF1$clustering,ct)

u1=max(u)-u

uz=HungarianSolver(u1)
score=0
for(i in 1:3){
  score=score+ u[uz$pairs[i,1],uz$pairs[i,2]]
  
  
}
score/length(ct)


###New method equation 4

start_time <- Sys.time()
a1=prox3(d,4)
end_time <- Sys.time()

end_time-start_time

D1 = sqrt(1-a1)
pamRF1 <- pam(D1, k = 3)
sil1=summary(silhouette(pamRF1$clustering,D1))
sil1$avg.width

adjustedRandIndex(pamRF1$clustering,ct)
NMI(pamRF1$clustering,ct)
u=table(pamRF1$clustering,ct)

u1=max(u)-u

uz=HungarianSolver(u1)
score=0
for(i in 1:3){
  score=score+ u[uz$pairs[i,1],uz$pairs[i,2]]
  
  
}
score/length(ct)


