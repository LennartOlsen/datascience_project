
soccer <- read.csv("normalized_data_no_first_five_ext.csv", header = TRUE)  ##Reads the CSV file and specifies that no header is present
#requires to source the base.r file
# No need for identifying variables a.k.a w_l_d_home
#soccer$w_l_d <- trim_dbData$w_l_d_home

#check available variables
colnames(soccer)

#check variable class for other than numeric type
str(soccer)

#divide the new data into train and test sets
set.seed(1)  #Keep this seed please

ind <- sample(2, nrow(soccer), replace=TRUE, prob=c(0.7, 0.3))

pca.train <- soccer[ind==1, 1:6]                                  #Extract the training set in accordination to the 1/2's from ind
pca.test <- soccer[ind==2, 1:6]                                      #Extract the training set in accordination to the 1/2's from ind

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

# 1. 
# center and scale refers to respective mean and standard deviation of the variables
# that are used for normalization prior to implementing PCA

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

# 2. 
# The rotation measure provides the principal component loading. 
# Each column of rotation matrix contains the principal component loading vector. 
# This is the most important measure we should be interested in.

prin_comp$rotation

# 3. 
# In order to compute the principal component score vector, we don’t need to multiply the loading with data. Rather, the matrix x has the principal component score vectors in a 8523 × 44 dimension.

dim(prin_comp$x)

#Let’s plot the resultant principal components.

biplot(prin_comp, scale = 0)


# 4. 
# The prcomp() function also provides the facility to compute standard deviation
# of each principal component. sdev refers to the standard deviation of principal components.

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:6]

#To compute the proportion of variance explained by each component, 
#we simply divide the variance by sum of total variance. This results in:
  
# proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:6]

# This shows that first principal component explains 35,7% variance. 
# Second component explains 34,7% variance
# Thrid component explains 10,5% variance

#scree plot
plot(prop_varex, xlab = "Principal Component",
      ylab = "Proportion of Variance Explained",
      type = "b")

# Let’s do a confirmation check, by plotting a cumulative variance plot. 
# This will give us a clear picture of number of components.

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")


# soccer.trainLabels <- soccer[ind==1, 7]                                      #Extract the labels accordingly
# soccer.testLabels <- soccer[ind==2, 7]