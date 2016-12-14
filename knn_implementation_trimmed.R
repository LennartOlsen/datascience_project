##
# kNN demo implementation
# STOLEN FROM : https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.9XuygoE
##

## install.packages("class") <- for the knn function
## install.packages("gmodels") <- for the CrossTable
## install.packages("ggvis") <- fot the awesome scatter plot
## library(ggvis)
library(class)
library(gmodels)

#Other method of normalization
soccer <- read.csv("norm_data_no_first_five_ext2.csv", header = TRUE)  ##Reads the CSV file and specifies that no header is present

#soccer <- read.csv("normalized_data_no_first_five_ext.csv", header = TRUE)  ##Reads the CSV file and specifies that no header is present
#requires to source the base.r file
soccer$w_l_d <- trim_dbData$w_l_d_home

##soccer %>% ggvis(~rank, ~form_5, fill = ~deficits_surplus) %>% layer_points() ##Look up ggvis, makes awesome scatter plots

# Finds best k by comparing accuracy of each k
range <- 1:20
accs <- rep(0, length(range))
sds <- rep(0, length(range))
runs <- 1:10
for (k in range) {
  for (run in runs){
    set.seed(run)  #Keep this seed please
    
    ind <- sample(2, nrow(soccer), replace=TRUE, prob=c(0.7,0.3))
    
    # test and training for knn
    soccer.training <- soccer[ind==1, 1:6]                                  #Extract the training set in accordination to the 1/2's from ind
    soccer.test <- soccer[ind==2, 1:6]                                      #Extract the training set in accordination to the 1/2's from ind
    soccer.trainLabels <- soccer[ind==1, 7]                                      #Extract the labels accordingly
    soccer.testLabels <- soccer[ind==2, 7]
    
    #make predictions using knn: pred
    pred <- knn(soccer.training, soccer.test, soccer.trainLabels, k = k)
    
    #construct the confusion matrix: conf
    conf <- table(soccer.testLabels, pred)
    
    #calculate the accuracy and store it in accs[k]
    sd <- c(acc, sum(diag(conf)) / sum(conf) )
    acc <- sum(diag(conf)) / sum(conf)
    print(run)
  }
  print(k)
  sds[k] <- sd(sd)
  accs[k] <- mean(acc)
}

require(ggplot2)
p <- qplot(seq_along(accs), accs, geom = "line" )+geom_errorbar(aes(x=seq_along(accs), ymin=accs-sds, ymax=accs+sds), width=0.1, alpha= I(1/2))
p <- p + xlab("K Value") + ylab("Accuracy")
p <- p + theme(panel.background = element_rect(fill="white"))
p <- p + theme(panel.grid.major = element_line(color="gray"))
p <- p + theme(axis.title.x = element_text(size = rel(2)))
p <- p + theme(axis.title.y = element_text(size = rel(2)))
p + theme(axis.text = element_text(size=rel(1.5)))