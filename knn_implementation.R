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

soccer <- read.csv("normalized_data.csv", header = TRUE)  ##Reads the CSV file and specifies that no header is present
summary(soccer)

soccer %>% ggvis(~rank, ~form_5, fill = ~deficits_surplus) %>% layer_points() ##Look up ggvis, makes awesome scatter plots

set.seed(1234)  #Keep this seed please

ind <- sample(2, nrow(soccer), replace=TRUE, prob=c(0.67, 0.33))  

soccer.training <- soccer[ind==1, 1:2]                                  #Extract the training set in accordination to the 1/2's from ind
soccer.test <- soccer[ind==2, 1:2]                                      #Extract the training set in accordination to the 1/2's from ind
soccer.trainLabels <- soccer[ind==1, 3]                                      #Extract the labels accordingly
soccer.testLabels <- soccer[ind==2, 3]


soccer_pred <- knn(train = soccer.training, test = soccer.test, cl = soccer.trainLabels, k=1)
soccer_pred

CrossTable(x = soccer.testLabels, y = soccer_pred, prop.chisq=FALSE)