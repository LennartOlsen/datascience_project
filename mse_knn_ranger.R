startTime = Sys.time()
range <- 1:100
mses <- rep(0, length(range))

for (k in range){
  soccer_pred <- knn(train = soccer.training, test = soccer.test, cl = soccer.trainLabels, k=k)
  mses[k] <- mse(soccer.testLabels, as.numeric(soccer_pred))
  print(k);
}

endTime = Sys.time()


print(endTime - startTime)
plot(range, mses, xlab = "k")
which.min(mses)
which.max(mses)