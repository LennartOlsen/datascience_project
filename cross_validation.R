### CROSS VALID ####
require(class)

set.seed(1)  #Keep this seed please

# test and training for knn
range <- 1:250
foldrange <- 1:10
accs <- rep(0, length(range))
sds <- rep(0, length(range))

for (k in range) {
  acc <- NULL
  for(fold in foldrange){
    minTest <- (fold - 1) *nrow(soccer)/max(foldrange)
    maxTest <- (fold) * nrow(soccer)/max(foldrange)
    subset.test <- soccer[ minTest : maxTest, 1:6]
    subset.test.labels  <- soccer[ minTest: maxTest, 7]
    
    #BUILD train SET
    soccer.train <- soccer[0 : minTest,  1:6]
    soccer.train <- rbind(soccer.train, soccer[maxTest : nrow(soccer), 1:6])
    soccer.train.labels <- soccer[0 : minTest, 7]
    soccer.train.labels <- c(soccer.train.labels, soccer[maxTest : nrow(soccer), 7])
    
    #make predictions using knn: pred
    pred <- knn(soccer.train, subset.test, cl = soccer.train.labels, k = k)
    
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
p <- p + xlab("K Value") + ylab("Accuracy")
p <- p + theme(panel.background = element_rect(fill="white"))
p <- p + theme(panel.grid.major = element_line(color="gray"))
p <- p + theme(axis.title.x = element_text(size = rel(2)))
p <- p + theme(axis.title.y = element_text(size = rel(2)))
p + theme(axis.text = element_text(size=rel(1.5)))
