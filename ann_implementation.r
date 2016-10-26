###
## REQUIRES "annTestData" which comes from base.R
###

#install.packages("caTools")
#install.packages('neuralnet')

#Define Scales
print(annTestData[,4:5,8])
traininginputMaxes <- apply(annTestData[,4:5],2,max)
traininginputMins <- apply(annTestData[,4:5],2,min)
scaled.data <- as.data.frame(scale(annTestData[,4:5],center = traininginputMins, scale = traininginputMaxes - traininginputMins))

#split into training and test set

annData = cbind(annTestData$w_l_d_away,scaled.data)

library(caTools)
set.seed(101)

# Create Split (any column is fine)
split = sample.split(annData$away_team_goal, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(annData, split == TRUE)
test = subset(annData, split == FALSE)

## SETUP FEATURES as formulae
feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('WLD_AWAY ~',f)

# Convert to formula
f <- as.formula(f)

f

library(neuralnet)
nn <- neuralnet(f,annData,hidden=c(10,10,10),linear.output=FALSE)
