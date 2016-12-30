#create function to calculate euqlidian distances
euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}
#create function for clustering
  K_means <- function(x, centers, distFun, nItter) {
    clusterHistory <- vector(nItter, mode="list")
    centerHistory <- vector(nItter, mode="list")
    
    for(i in 1:nItter) {
      distsToCenters <- distFun(x, centers)
      clusters <- apply(distsToCenters, 1, which.min)
      centers <- apply(x, 2, tapply, clusters, mean)
      # Saving history
      clusterHistory[[i]] <- clusters
      centerHistory[[i]] <- centers
    }

    list(clusters=clusterHistory, centers=centerHistory)
  }
#use iris data
test=iris[, 1:4] # A data.frame
#turn into a matrix
ktest=as.matrix(test) 
#sample some centers, 3 for example
centers <- ktest[sample(nrow(ktest), 3),] 
#itarate clustering for 5 times
res <- K_means(ktest, centers, euclid, 5)
#obtain points number inside clusters
do.call(rbind, lapply(res$clusters, function(x){table(x)}))
#merge clusters to data.frame
clusters <- do.call(rbind, lapply(res$clusters, function(x){x}))
#obtain the most frequent cluster for each point
myclusters <- apply(clusters, 2, function(x){names(which.max(table(x)))})
#convert to factor
myclusters <- factor(myclusters, levels = c(1,2,3))
#call library for 3d plotting
library(scatterplot3d)
#divide plot into 2 columns
par(mfrow = c(1,2))
#plot original
scatterplot3d(iris[, 1], iris[, 2], iris[, 3], 
              color = as.numeric(iris$Species), pch = 16, xlab = 'Sepal Length', 
              ylab = 'Sepal Width', zlab = "Petal Length", main = 'Original groups by Species')
#plot clustering
scatterplot3d(iris[, 1], iris[, 2], iris[, 3], color = as.numeric(myclusters), pch = 1, xlab = 'Sepal Length', 
              ylab = 'Sepal Width', zlab = "Petal Length", main = 'Groups by clustering')
#calculate accuracy
sum(diag(table(myclusters, iris$Species))) / sum(table(myclusters, iris$Species))*100

#Result of running the code differs, as we use low number of iterations. 
#Sometimes we obtain clusters which are rather close to natural, sometimes we obtain unsuspected results. 
#To increase clustering accuracy we should increase iterations number.
