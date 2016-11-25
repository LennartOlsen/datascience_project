#install.packages("Metrics")
library(Metrics)
startTime = Sys.time()
range <- 1:300
mses <- rep(0, length(range))

for (k in range){
  pca_pred <- knn(train = pca.training, test = pca.test, cl = pca.trainLabels, k=k)
  mses[k] <- mse(pca.testLabels, as.numeric(pca_pred))
  print(k);
}

endTime = Sys.time()


print(endTime - startTime)
plot(range, mses, xlab = "k")
which.min(mses)
which.max(mses)