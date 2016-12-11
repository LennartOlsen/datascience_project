### CROSS VALID ####
require(class)

set.seed(1)  #Keep this seed please

range <- 1:300
foldrange <- 1:10
accs <- rep(0, length(range))
sds <- rep(0, length(range))

for (k in range) {
  acc <- NULL
  for(fold in foldrange){
    
    minTest <- (fold - 1) *nrow(pca)/max(foldrange)
    maxTest <- (fold) * nrow(pca)/max(foldrange)
    subset.test <- pca[ minTest : maxTest, 1:4]
    subset.test.labels  <- pca[ minTest: maxTest, 5]
    
    #BUILD train SET
    subset.train <- pca[0 : minTest,  1:4]
    subset.train <- rbind(subset.train, pca[maxTest : nrow(pca), 1:4])
    subset.train.labels <- pca[0 : minTest, 5]
    subset.train.labels <- c( subset.train.labels, pca[maxTest : nrow(pca), 5])
    
    #make predictions using knn: pred
    pred <- knn(subset.train, subset.test, cl = subset.train.labels, k = k)
    
    #construct the confusion matrix: conf
    conf <- table(subset.test.labels, pred)
    #calculate the accuracy and store it in accs[k]
    acc <- c(acc, sum(diag(conf)) / sum(conf) )
    print(fold)
  }
  print(k)
  print('finito')
  sds[k] <- sd(acc)
  accs[k] <- mean(acc)
}
print(sds)
print(accs)

which.max(accs)

require(ggplot2)
p <- qplot(seq_along(accs), accs, geom = "line" )+geom_errorbar(aes(x=seq_along(accs), ymin=accs-sds, ymax=accs+sds), width=0.1, alpha= I(1/2))
p + xlab("K Value") + ylab("Accuracy")
