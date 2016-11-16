##
# kNN demo implementation
# STOLEN FROM : https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.9XuygoE
##

## install.packages("class") <- for the knn function
## install.packages("gmodels") <- for the CrossTable
## install.packages("ggvis") <- fot the awesome scatter plot
library(ggvis)
library(class)
library(gmodels)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

soccer <- read.csv("normalized_data_no_first_five.csv", header = TRUE)  ##Reads the CSV file and specifies that no header is present
#requires to source the base.r file
soccer$w_l_d <- trim_dbData$w_l_d_home
summary(soccer)

##soccer %>% ggvis(~rank, ~form_5, fill = ~deficits_surplus) %>% layer_points() ##Look up ggvis, makes awesome scatter plots

set.seed(1234)  #Keep this seed please

ind <- sample(2, nrow(soccer), replace=TRUE, prob=c(0.70, 0.3))  

soccer.training <- soccer[ind==1, 1:2]                                  #Extract the training set in accordination to the 1/2's from ind
soccer.test <- soccer[ind==2, 1:2]                                      #Extract the training set in accordination to the 1/2's from ind
soccer.trainLabels <- soccer[ind==1, 4]                                      #Extract the labels accordingly
soccer.testLabels <- soccer[ind==2, 4]


soccer_pred <- knn(train = soccer.training, test = soccer.test, cl = soccer.trainLabels, k=4)
soccer_pred

table(soccer.testLabels,soccer_pred)

# Finds best k by comparing accuracy of each k
# range <- 1:100
# accs <- rep(0, length(range))
# 
# for (k in range) {
#   
#   #make predictions using knn: pred
#   pred <- knn(soccer.training, soccer.test, soccer.trainLabels, k = k)
#   
#   #construct the confusion matrix: conf
#   conf <- table(soccer.testLabels, pred)
#   
#   #calculate the accuracy and store it in accs[k]
#   accs[k] <- sum(diag(conf)) / sum(conf)
#   print(k)
# }
# 
# # Plot the accuracies. Title of x-axis is "k".
# plot(range, accs, xlab = "k")
# 
# # Calculate the best k
# which.max(accs)