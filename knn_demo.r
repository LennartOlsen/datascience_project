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

iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)  ##Reads the CSV file and specifies that no header is present
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")                         ##Adds names to the CSV file

iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points() ##Look up ggvis, makes awesome scatter plots
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points() ##Using the petal instead of spetal

summary(iris)

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

summary(iris_norm)

set.seed(1234)                                                                #sets the seed so that we are always sure that the data is the same

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))                #Create 150 random numbers, with a propability of 0.67 of 1 and 0.33 of 2

print(ind) #Ergo 2/3's of the numbers are 1s and the rest is 2s

iris.training <- iris_norm[ind==1, 1:4]                                  #Extract the training set in accordination to the 1/2's from ind
iris.test <- iris_norm[ind==2, 1:4]                                      #Extract the training set in accordination to the 1/2's from ind
iris.trainLabels <- iris[ind==1, 5]                                      #Extract the labels accordingly
iris.testLabels <- iris[ind==2, 5]

summary(iris)

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=1)

iris_pred

CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)